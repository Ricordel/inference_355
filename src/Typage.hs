module Typage where


import Parsage
import Text.ParserCombinators.Parsec



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
    typeof (Const _ t) = t
    typeof (Var _ t) = t
    typeof (Un _ _ t) = t
    typeof (Bin _ _ _ t) = t


    -- Inférer le type
    infer_type (Var x t) = Var x t -- FIXME : ira de pair avec le support de LET

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

    -- Pour le reste, rien besoin de faire, le type a déjà été inféré
    infer_type e = e



    -- Propager le type d'une variable
    propag_type var t (Var var' (Just [])) =
        if var == var' then
            Var var' t
        else
            Var var' (Just [])

    -- Pour les autres, il ne s'agit que de bêtes appels récursifs
    propag_type var t c@(Const _ _) = c

    propag_type var t (Un op e t_e) = let e' = propag_type var t e in Un op e' t_e

    propag_type var t (Bin op e1 e2 t_e) =
        let e1' = propag_type var t e1
            e2' = propag_type var t e2
        in
            Bin op e1' e2' t_e



-- Juste pour tester
main = let ast = parse stmt "" "let e = fun(x,y) : (x - y) * 4 in f(a,b,c) + 5"
        in
            case ast of 
                Left err -> print err

                Right arbre -> putStrLn $ show arbre
