module Typage where


import Parsage
import Text.ParserCombinators.Parsec
import Control.Monad

import System.Environment

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
    -- propage le type d'une Var ... ou FunDef ... au sous-arbre
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


    
    propag_type iden t e =
        case e of 
            If cond s1 s2 t_if ->
                let cond' = propag_type iden t cond
                    s1' = propag_type iden t s1
                    s2' = propag_type iden t s2
                in
                    If cond' s1' s2' t_if

            Expr e t_e ->
                let e' = propag_type iden t e
                in
                    Expr e' t_e

            Let name val s t_let ->
                let val' = propag_type iden t val
                    s' = propag_type iden t s
                in
                    Let name val' s' t_let





instance Typed Expr where

    -- Obtenir le type
    typeof (Const _ t)      = t
    typeof (Var _ t)        = t
    typeof (Un _ _ t)       = t
    typeof (Bin _ _ _ t)    = t
    typeof (FunCall _ _ t)  = t
    typeof (FunDef _ _ t)   = t


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
        let def' = infer_type $ infer_var_types def
            t = map (type_of_arg def') args -- :: [Type] = [Maybe [SimpleType]]
            t' = liftM concat $ sequence (t ++ [typeof def'])  -- sequence ==> Maybe [[SimpleType]], sauf que la liste la plus interne 
                                            -- contient des types de variables (donc types simples, un seul élément)
                                            -- -> on peut "applatir" avec concat
            
        in
            FunDef args def' t'

            -- Là où on aime les MonadPlus :-)
            where 
                type_of_arg def arg =
                    case def of
                            -- Attention, les types des variables doivent déjà être inférés
                            -- dans la définition examinée
                        Var name t -> if name == arg then  t else Nothing
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
    --        qui devrait avoir deux types)
    

    -- Pour le reste, rien besoin de faire, le type a déjà été inféré
    infer_type e = e






    -- Propager le type d'une variable ou d'une fonction bindée par let
    --
    -- Deux cas où on fait "vraiment" quelque chose :
    --      - une variable avec le même nom
    --      - une fonction avec le même nom
    --
    -- TODO : mettre un joli message d'erreur dans le cas otherwise au lieu de juste retourner Nothing

    propag_type iden (Just t) v@(Var name (Just []))
        | iden == name = 
            if length t == 1 then -- il nous faut une variable
                Var name (Just t)
            else
                Var iden Nothing -- var avec type composé -> interdit (pas d'appli partielle pour l'instant)
        | otherwise = v
            
    -- pour les variables déjà typées
    propag_type iden t v@(Var _ _) = v

    propag_type iden (Just t) (FunCall (f, Just []) args t_call)
        | iden == f =
            if length t == length args + 1 then
                let args' = map (propag_type iden (Just t)) args
                in
                    FunCall (f, Just t) args' t_call
            else -- application partielle, non supportée pour l'instant
                FunCall (f, Nothing) args Nothing

        | otherwise =
            let args' = map (propag_type iden (Just t)) args
            in
                FunCall(f, Just t) args' t_call

    -- Pour les fonctions déjà typées
    propag_type iden t (FunCall f args t_call) =
        let args' = map (propag_type iden t) args
        in
            FunCall f args' t_call

    -- Tous les autres cas sont de simples appels récursifs
    propag_type iden t c@(Const _ _) = c

    propag_type iden t (Un op e t_rslt) =
        let e' = propag_type iden t e
        in Un op e' t_rslt

    propag_type iden t (Bin op e1 e2 t_rslt) =
        let e1' = propag_type iden t e1
            e2' = propag_type iden t e2
        in
            Bin op e1' e2' t_rslt
            
    propag_type iden t (FunDef args def t_fun) =
        let def' = propag_type iden t def
        in
            FunDef args def' t_fun






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
                Un (op, Just t_op) (Var x t_var) t

        Un op e t ->
            let e' = infer_var_types e
            in
                Un op e' t

        Bin (op, Just t_op) (Var x (Just [])) (Var y (Just [])) t ->
            let t_x = Just [t_op !! 1]
                t_y = Just [t_op !! 2]
            in
                Bin (op, Just t_op) (Var x t_x) (Var y t_y) t

        Bin (op, Just t_op) (Var x (Just [])) y t ->
            let t_x = Just [t_op !! 1]
                y' = infer_var_types y
            in
                Bin (op, Just t_op) (Var x t_x) y' t

        Bin (op, Just t_op) x (Var y (Just [])) t ->
            let t_y = Just [t_op !! 2]
                x' = infer_var_types x
            in
                Bin (op, Just t_op) x' (Var y t_y) t

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
                    args' = map type_vars types_and_args -- FIXME : un argument de fonction n'est pas forcément Var ou Const...
                in
                    FunCall (f, Just t_f) args' t_rslt
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
{-main = let ast = parse stmt "" "let f = fun(x,y) : (x - y) * 4 in if True then let x = 4 in x+2 <= 3 else if (True && False) then f(5, 4) == 2 else True"-}
{-main = let ast = parse stmt "" "let f = fun(x,y) : (x - y) * 4 in if True then let x = 0 in x + f(4, 5) else  4"-}
{-main = let ast = parse stmt "" "let f = fun(x,y) : (x - y) in let a = b in 4"-}

main = do
        file_name : other_args <- getArgs
        rslt <- parseFromFile stmt file_name
        case rslt of
            Left err -> print err
            Right ast -> (putStrLn (show ast)) >> (putStrLn "\n\n") >> (putStrLn (show $ infer_type ast))

                {-Right arbre -> (putStrLn $ show arbre) >> (putStrLn "\n\n") >>-}
                               {-(putStrLn $ show (infer_type arbre))-}



-- Les problèmes du moment : 
--      - grammaire récursive à gauche -> boucle infinie lorsqu'on imbrique des let
--        (or je veux absolument pouvoir imbriquer des let !)
--      - on ne peut pas utiliser une fonction comme argument d'une autre fonction,
--        car le typage échoue ==> Normal, on appelle type_vars pour typer les arguments...
--      - Changer la précédence de == par rapport à && et ||
