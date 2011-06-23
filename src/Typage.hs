module Typage where


import Parsage
import Text.ParserCombinators.Parsec
import Control.Monad

import Debug.Trace



-- Maintenant on va propager nos types
-- et vérifier leur cohérence.
--
-- Pour un If expr s1 s2 t, il faut
--      - type(expr) = Bool
--      - type(s1) = type(s2)
--      - on en déduit alors le type du statement : t <- type(s1)
--
-- Pour un Let var val stmt
--      - on infère le type de val, puis on le propage aux différentes occurences
--        de var dans stmt



-- TODO  la gestion des erreurs est pour l'instant minimale : Nothing en cas d'échec, à améliorer

-- TODO : Vérifier qu'il n'y a pas de redéfinition de variable avec le même identificateur

-- On factorise le caractère typé de nos statements et expressions en une classe
class Typed a where
    typeof :: a -> Type
    infer_type :: a -> a
    -- utilisé pour propager le type d'une variable définie avec un let
    propag_type :: String -> Type -> a -> a



instance Typed Statement where

    -- Obtenir le type calculé jusqu'à présent
    typeof (If _ _ _ t) = t
    typeof (Let _ _ _ t) = t
    typeof (Expr e t) = t


    -- Inférer le type d'un statement
    infer_type (If cond s1 s2 (Just [])) =
        let cond' = infer_type cond
            s1' = infer_type s1
            s2' = infer_type s2
        in
            if typeof cond' == Just [Bool] && typeof s1' == typeof s2' then
                If cond' s1' s2' (typeof s1')
            else
                If cond' s1' s2' Nothing

    infer_type (Expr e (Just [])) = let e' = infer_type e in Expr e' (typeof e')

    infer_type (Let var val s (Just [])) = 
        let val' = infer_type val
            s' = infer_type $ propag_type var (typeof val') s
        in
            Let var val' s' (typeof s')


    
    -- Propager le type d'une variable
    propag_type var t (If cond s1 s2 t_if) =
        let cond' = propag_type var t cond
            s1' = propag_type var t s1
            s2' = propag_type var t s2
        in
            If cond' s1' s2' t_if

    propag_type var t (Expr e t_e) =
        let e' = propag_type var t e
        in
            Expr e' t_e

    propag_type var t (Let iden val s t_let) =
        let val' = propag_type var t val
            s' = propag_type var t s'
        in
            Let iden val' s' t_let
    
    


instance Typed Expr where

    -- Obtenir le type
    typeof arg@(Const _ t)      = t
    typeof arg@(Var _ t)        = t
    typeof arg@(Un _ _ t)       = t
    typeof arg@(Bin _ _ _ t)    = t
    typeof arg@(FunCall _ _ t)  = t
    typeof arg@(FunDef _ _ t)   = t


    -- Inférer le type
    infer_type (Var x t) = Var x t -- Il n'y a rien à inférer, c'est fait par propagation du type de la variable déclarée

    infer_type (Un (op, Just t_op) e (Just [])) = 
        let e' = infer_type e
            t_arg : t_rslt = t_op
        in
        if typeof e' == Just [t_arg] then
            Un (op, Just t_op) e' (Just t_rslt)
        else
            Un (op, Just t_op) e' Nothing
        

    infer_type (Bin (op, Just t_op) e1 e2 (Just [])) =
        let e1' = infer_type e1
            e2' = infer_type e2
            t_arg1 : t_arg2 : t_rslt = t_op
        in
        if typeof e1' == Just [t_arg1] && typeof e2' == Just [t_arg2] then
            Bin (op, Just t_op) e1' e2' (Just t_rslt) -- Ben pris en compte ici
        else
            Bin (op, Just t_op) e1' e2' Nothing


    -- FIXME est-il possible ici que t_f soit Nothing ?
    infer_type (FunCall (f, Just t_f) args (Just [])) = 
    -- FIXME : ça sent la merde : quel besoin de faire infer_var_types ?! Ah si ! pour vérifier que les types
    -- sont les bons
        let args' = map infer_var_types args -- inférer le type des variables qui n'en ont pas encore,
                                             -- typiquement des variables liées par un lambda. Tous les args' sont typés
            types_and_args = zip args' t_f
            valid = and $ map (\(a, t) -> Just [t] == typeof a) types_and_args -- les arguments ont le bon type
        in
            if valid then
                FunCall (f, Just t_f) args' (Just (snd $ splitAt (length args) t_f))
            else
                FunCall (f, Just t_f) args' Nothing


    -- Il faut aller chercher, pour chacun des arguments, son type dans le
    -- sous-arbre. Dans un premier temps, on supposera que le programme est correct et qu'aucun argument
    -- n'est utilisé avec deux types différents.
    -- TODO : faire échouer si on a plusieurs types différents pour une var dans le corps de la fonction
    infer_type (FunDef args def (Just [])) =
        let def' = trace ("\nINFER_TYPE (FunDef) : va appeler infer_var_types avec " ++ show def ++ "\n\n") (infer_var_types $ infer_type def)
            t = trace ("\n\nThe result was : " ++ show def' ++ "\n\n") (map (type_of_arg def') args) -- :: [Type] = [Maybe [SimpleType]]
            t' = liftM concat $ sequence t  -- sequence ==> Maybe [[SimpleType]], sauf que la liste la plus interne 
                            -- contient des types de variables (donc types simples) -> on peut "applatir" avec concat
            
        in
            FunDef args def' t'

            -- Là où on aime les MonadPlus :-)
            where 
                type_of_arg def arg =
                    case def of
                            -- Attention, les types des variables doivent déjà être inférés
                            -- dans la définition examinée
                        Var name t -> if name == arg then trace ("TYPE_OF_ARGS (VAR) Just : " ++ show t ++ ", " ++ name) t else trace ("TYPE_OF_ARGS (VAR - Nothing) " ++ name)  Nothing
                        Un _ e _ -> type_of_arg e arg
                        Bin _ e1 e2 _ -> type_of_arg e1 arg `mplus` type_of_arg e2 arg
                        FunCall _ fun_args _ ->
                            let pot_types = map (\e -> type_of_arg e arg) fun_args
                            in
                                msum pot_types

                        FunDef _ _ _ -> Nothing
            
    -- Inférer le type d'une lambda est le plus tricky de ce qu'il y a jusqu'à présent. Il faut :
    --      - inférer le type des variables en regardant leurs apparitions dans la 
    --        définition de la fonction, en vérifiant qu'il n'y a pas de conflit (une variable
    --        qui devrait avori deux types)
    

    -- Pour le reste, rien besoin de faire, le type a déjà été inféré
    infer_type e = e






    -- Propager le type d'une variable
    propag_type var t (Var var' (Just [])) =
        if var == var' then
            Var var' t
        else
            Var var' (Just [])

    propag_type var t (Un op e t_e) = let e' = propag_type var t e in Un op e' t_e

    propag_type var t (Bin op e1 e2 t_e) =
        let e1' = propag_type var t e1
            e2' = propag_type var t e2
        in
            Bin op e1' e2' t_e

    propag_type var t (FunCall f args t_f) =
        let args' = map (propag_type var t) args
        in
            FunCall f args' t_f

    propag_type var t (FunDef f e t_f) =
        let e' = propag_type var t e
        in
            FunDef f e' t_f

    -- Dans les autres cas (const, var djà typée), ne rien faire
    propag_type var t x = x


-- infère le type des variables à partir de leur utilisation
--
-- Et les cas récursifs avec d'autres expressions ??!! Faudrait peut-être les faire !
--
infer_var_types :: Expr -> Expr
infer_var_types arbre =
    case arbre of
        Un (op, Just t_op) (Var x (Just [])) t -> 
            let t_var = Just [t_op !! 1]
            in
                trace ("INFER_VAR_TYPES (Un) : a inféré le type " ++ show t_var ++ " pour la variable " ++ x)
                (Un (op, Just t_op) (Var x t_var) t)

        Un op e t ->
            let e' = infer_var_types e
            in
                Un op e' t

        Bin (op, Just t_op) (Var x (Just [])) (Var y (Just [])) t ->
            let t_x = Just [t_op !! 1]
                t_y = Just [t_op !! 2]
            in
                trace ("INFER_VAR_TYPES (Bin) : a inféré le type " ++ show t_x ++ " pour la variable " ++ x
                ++ "INFER_VAR_TYPES (Bin) : a inféré le type " ++ show t_y ++ " pour la variable " ++ y)
                (Bin (op, Just t_op) (Var x t_x) (Var y t_y) t)

        Bin (op, Just t_op) (Var x (Just [])) y t ->
            let t_x = Just [t_op !! 1]
            in
                trace ("INFER_VAR_TYPES (Bin) : a inféré le type " ++ show t_x ++ " pour la variable " ++ x)
                (Bin (op, Just t_op) (Var x t_x) y t)

        Bin (op, Just t_op) x (Var y (Just [])) t ->
            let t_y = Just [t_op !! 2]
            in
                trace ("INFER_VAR_TYPES (Bin) : a inféré le type " ++ show t_y ++ " pour la variable " ++ y)
                (Bin (op, Just t_op) x (Var y t_y) t)

        Bin op e1 e2 t ->
            let e1' = infer_var_types e1
                e2' = infer_var_types e2
            in
                Bin op e1' e2' t


        FunCall (f, Just t_f) args t_rslt ->
            if length args > length t_f then
                FunCall (f, Just t_f) args Nothing
            else
                let types_and_args = zip t_f args
                    args' = map type_vars types_and_args
                in
                    trace ("INFER_VAR_TYPES (FunCall) : a inféré un type ")
                    (FunCall (f, Just t_f) args' t_rslt)
                where
                    -- On type les variables liées par lambda, donc celles qui n'ont pas encore de type
                    -- (les autres en ont une par propagation de let dans le processus d'inférence)
                    type_vars (t, Var x (Just [])) = Var x (Just [t])
                    type_vars (t, x) = x

        -- si le type n'est pas Just [], la var a déjà été typée par propagation
        _ -> arbre

        ----- Normalement, après passage d'une expression dans cette fonction, toutes les variables
        -- liées par des let ont été typées par propagation (il faudra appeler infer_type expr !), et
        -- tous les types des variables liées dans la lambda ont été déduit de leur contexte d'utilisation
        -- TODO : mettre cette fonction au bon endroit ! (peut-être une locale tout simplement...)
        




-- Juste pour tester
main = let ast = parse stmt "" "let f = fun(x,y) : (x - y) * 4 in f(4,2) + 5"
        in
            case ast of 
                Left err -> print err

                Right arbre -> {-(putStrLn $ show arbre) >> (putStrLn "\n\n") >>-}
                               (putStrLn $ show (infer_type arbre))
