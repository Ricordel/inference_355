module Typage where


import Parsage
import Text.ParserCombinators.Parsec
import Control.Monad
import Data.List
import Data.Maybe

import System.Environment

import Debug.Trace






-- TODO : Vérifier qu'il n'y a pas de redéfinition de variable avec le même identificateur
-- TODO : Gestion lisible et un minimum précise des erreurs
-- TODO : faire une sortie textuelle digeste


-- La manière d'inférer, vérifier et propager les types dans ce programme est la suivante :
--      Pour un if cond then s1 else s2 :
--          - inférer le type de cond
--          - inférer le type de s1
--          - inférer le type de s2
--          - en déduire le type de tout le statement
--          - (un petit cas part si c'est à l'intérieur d'une déf de fonction, cf cas let)
--
--      Pour une Var, deux cas :
--          - soit c'est dans un let, inférer son type à partir de sa définition, propager ce type
--          - soit ce n'est pas dans un let, alors elle doit avoir été bindée par un let, et
--            son type a été propagé
--
--      Pour une Const :
--          - rien à faire, le type a été mis lors de parsage. On pourrait imaginer ne pas le faire
--            lors du parsage et rajouter une première passe de typage qui donne le type des variables.
--            Pour cette version limitée, je ne l'ai pas jugé nécessaire
--
--      Pour un Un op e :
--          - inférer le type de e
--          - vérifier qu'il est cohérent avec le type de op (les types des opérateurs sont aussi mis
--            dans l'arbre dès la phase de parsage)
--          - déduire alors du type de op le type de son application à e
--
--      Pour un Bin op e1 e2 :
--          - inférer le type de e1 et e2
--          - vérifier qu'ils sont compatibles avec le type de op
--          - déduire alors du type de op le type de son application à e1 et e2
--
--      Pour un FunCall f [args] :
--          - inférer le type des args
--          - vérifier qu'ils sont compatibles avec le type de f (qui aura été propagé lors de sa défintion)
--          - en déduire le type de l'appel
--
--      Pour un let var = val in portée :
--          - inférer le type de val
--          - dire que c'est le type de var
--          - propager ce type dans le sous-arbre "portée"
--
--      Pour un FunDef [args] déf,
--          - inférer le type des args à partir de déf
--          - propager à nouveau le type des args dans la définition. C'est nécessaire par exemple pour :
--            let max = fun(x, y) : if x < y then y else x
--            le type de x est inféré dans la condition (x < y), mais certainement pas dans le then x, ou le else y
--            Une fois un type trouvé pour chaque argument, on doit donc le propager à toute la définition pour
--            pouvoir connaître le type global de cette définition
--          - du type des arguments et de la définition, déduire le type de la fonction
--
--
--
-- Nous allons donc utiliser les fonctions suivantes :
--      - typeof : retourne simplement le type de l'expression, sans rien inférer de plus
--      - propag_type : propage le type d'une variable ou d'une fonction dans le sous-arbre correspondant
--        à la portée de sa définition
--      - infer_args : utilisé pour inférer le type des paramètres d'une fonction à partir de leur utilisation
--      - typeof_arg : qui retourne chercher le type des paramètres précédemment inférés
--      - infer_type : qui infère récursivement à l'aide des fonctions précédentes le type de tout l'arbre



-- Comme les Statements et Expr ont besoin de la même "interface", on les regroupe dans une type class

class Typed a where
    typeof :: a -> Type
    propag_type :: String -> Type -> a -> a
    infer_args :: a -> a
    type_of_arg :: a -> String -> Type
    infer_type :: a -> a






instance Typed Statement where

    -- Obtenir le type calculé jusqu'à présent
    typeof (If _ _ _ t) = t
    typeof (Let _ _ _ t) = t
    typeof (Expr e t) = t

    -- infer_type n'a quelque chose à faire que si le type du statement à inférer est Just [], cas
    --      - si c'est Nothing, le typage a déjà été fait et a échoué
    --      - si c'est autre chose, le typage a déjà été fait et à réussi, pas la peine de recommencer
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

    -- dans les autres cas : identité
    infer_type other = other

    
    -- propager le type d'un identificateur dans les différents types de statement
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

            Let name val s t_let -> -- faire échouer si la variable est redéfinie
                if iden == name then
                    Let name val s Nothing
                else
                let val' = propag_type iden t val
                    s' = propag_type iden t s
                in
                    Let name val' s' t_let



    -- l'inférence des arguments, également de simples appels récursifs
    infer_args (Expr e t) = let e' = infer_args e in Expr e' t

    infer_args (If cond e1 e2 t) =
        let cond' = infer_args cond
            e1' = infer_args e1
            e2' = infer_args e2
        in
            If cond' e1' e2' t

    infer_args (Let var val s t) =
        let val' = infer_args val
            s' = infer_args s
        in
            Let var val' s' t



    -- Faire remonter le type des arguments précédemment inférés
    -- Comme on l'a vu, le type peut n'être inféré que pour une occurence de l'argument et être
    -- évalué à Nothing ailleurs. L'important est qu'au moins un type soit différent de Nothing
    -- et que tous les types différents de Nothing soient identiques.
    type_of_arg def arg =
       case def of
           Expr e t_e -> type_of_arg e arg

           If cond e1 e2 t_cond ->
                let types = [type_of_arg cond arg, type_of_arg e1 arg, type_of_arg e2 arg]
                    types' = nub $ filter (/= Just []) $ filter isJust types

                in
                    if length types' == 1 then
                        head types'
                    else
                        Nothing


           Let var val s t_let ->
                let types = [type_of_arg val arg, type_of_arg s arg]
                    types' = nub $ filter (/= Just []) $ filter isJust types
                in
                    if length types' == 1 then
                        head types'
                    else
                        Nothing








instance Typed Expr where

    typeof (Const _ t)      = t
    typeof (Var _ t)        = t
    typeof (Un _ _ t)       = t
    typeof (Bin _ _ _ t)    = t
    typeof (FunCall _ _ t)  = t
    typeof (FunDef _ _ t)   = t





    infer_type v@(Var x t) = v -- Il n'y a rien à inférer, c'est fait par propagation du type de la variable déclarée

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
            Bin (op, Just t_op) e1' e2' (Just t_rslt)
        else
            Bin (op, Just t_op) e1' e2' Nothing


    infer_type (FunCall (f, Just t_f) args (Just [])) = 
        let args' = map infer_type args
            types_and_args = zip args' t_f
            valid = and $ map (\(a, t) -> Just [t] == typeof a) types_and_args -- les arguments ont le bon type
        in
            if valid then
                FunCall (f, Just t_f) args' (Just (snd $ splitAt (length args) t_f))
            else
                FunCall (f, Just t_f) args' Nothing


    -- Ici, on doit effectuer plusieurs étapes :
    --      - inférer le type des arguments dans la définition (-> t_args, def')
    --      - propager ce type à TOUTE la définition (problème du if then else discuté
    --        au début de ce fichier) -> def''
    --      - inférer le type de la def''
    --      - de tout ceci, déduire le type de f
    infer_type (FunDef args def (Just [])) =
        let def' = infer_type $ infer_args def
            t_args = map (type_of_arg def') args

            -- construisons une fonction pour propager les types des arguments
            l1 = map propag_type args -- fonctions partielles propag_type arg
            l2 = zipWith ($) l1 t_args -- fonctions partielles propag_type arg t_arg
            propag_fun = foldl (.) id l2 -- on enchaîne ces fonctions
            def'' = infer_type $ propag_fun def'

            -- on peut maintenant en déduire le type de la fonction
            -- t_args = [Just [t1], Just [t2], ..., Just [tn]]
            t_args' = sequence $ t_args ++ [typeof def''] -- Just [[t1], [t2], ..., [tn]]
            t_args'' = liftM concat $ t_args' -- Just [t1, t2, ..., tn]
        in
            FunDef args def'' t_args''


    -- Pour le reste, rien besoin de faire, le type a déjà été inféré
    infer_type e = e






    -- Propager le type d'une variable ou d'une fonction bindée par let
    --
    -- Deux cas où on fait "vraiment" quelque chose :
    --      - une variable avec le même nom
    --      - une fonction avec le même nom

    propag_type iden (Just t) v@(Var name (Just []))
        | iden == name = 
            if length t == 1 then -- il nous faut une variable
                Var name (Just t)
            else
                Var iden Nothing -- var avec type composé -> interdit (pas d'appli partielle)
        | otherwise = v
            
    -- pour les variables déjà typées, on vérifie qu'on n'a pas de clash
    propag_type iden t v@(Var name t_var)
        | iden == name && t /= t_var = 
                Var name Nothing -- clash !
        | otherwise = v


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
                FunCall(f, Just []) args' t_call

    -- Pour les fonctions déjà typées, on vérifie qu'il n'y a pas de clash
    propag_type iden t fun@(FunCall (f, t_f) args t_call)
        | iden == f && t /= t_f =
            let args' = map (propag_type iden t) args
            in
                FunCall (f, Nothing) args' Nothing -- clash
        | otherwise =
            let args' = map (propag_type iden t) args
            in
                FunCall (f, t_f) args' t_call


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
    infer_args arbre =
        case arbre of
            Un (op, Just t_op) (Var x (Just [])) t -> 
                let t_var = Just [t_op !! 0]
                in
                    Un (op, Just t_op) (Var x t_var) t

            Un op e t ->
                let e' = infer_args e
                in
                    Un op e' t

            Bin (op, Just t_op) (Var x (Just [])) (Var y (Just [])) t ->
                let t_x = Just [t_op !! 0]
                    t_y = Just [t_op !! 1]
                in
                    Bin (op, Just t_op) (Var x t_x) (Var y t_y) t

            Bin (op, Just t_op) (Var x (Just [])) y t ->
                let t_x = Just [t_op !! 0]
                    y' = infer_args y
                in
                    Bin (op, Just t_op) (Var x t_x) y' t

            Bin (op, Just t_op) x (Var y (Just [])) t ->
                let t_y = Just [t_op !! 1]
                    x' = infer_args x
                in
                    Bin (op, Just t_op) x' (Var y t_y) t

            Bin op e1 e2 t ->
                let e1' = infer_args e1
                    e2' = infer_args e2
                in
                    Bin op e1' e2' t


            FunCall (f, Just t_f) args t_rslt ->
                if length args /= length t_f - 1 then -- pas d'application partielle
                    FunCall (f, Just t_f) args Nothing
                else
                    let types_and_args = zip t_f args
                        args' = map type_vars types_and_args
                    in
                        FunCall (f, Just t_f) args' t_rslt
                    where
                        -- type les variables pas encore typées en fonction de leur utilisation dans cette fonction
                        type_vars (t, Var x (Just [])) = Var x (Just [t])
                        type_vars (t, x) = x


            -- dans les autres cas d'exression, rien à faire
            _ -> arbre


    type_of_arg def arg =
        case def of
            -- Attention, les types des variables doivent déjà être inférés
            -- dans la définition examinée
            Var name t -> if name == arg then t else Nothing
            Un _ e _ -> type_of_arg e arg
            Bin _ e1 e2 _ -> 
                let types = [type_of_arg e1 arg, type_of_arg e2 arg]
                    types' = nub $ filter (/= Just []) $ filter isJust types
                in
                    if length types' == 1 then
                        head types'
                    else
                        Nothing


            FunCall _ fun_args _ ->
                let types = map (\e -> type_of_arg e arg) fun_args
                    types' = nub $ filter (/= Just []) $ filter isJust types
                in
                    if length types' == 1 then
                        head types'
                    else
                        Nothing


            FunDef _ _ _ -> Nothing

        




