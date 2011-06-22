module Typage where


import Parsage
import Text.ParserCombinators.Parsec

-- Pour commencer, afin de pouvoir n'utiliser que l'AST et ne pas garder une table des symboles
-- à côté (par souci de simplicité), on élimine les constructions de type let x = truc in machin
-- en remplaçant toutes les apparitions de "Var x" par l'expression "truc" dans le sous-arbre "machin"
-- Ce ne serait pas efficace pour de la vraie compilation, ce n'est pas non plus optimal en efficacité pour
-- du typage, mais ça me semble plus ismple à gérer dans un premier temps






{-main = let ast = parse stmt "" "let e = 3 in 19 + e + 5 + 6 + 7 -5"-}



-------------------------------------------------------------------
--  Maintenant, décorons not noeud et feuilles avec leurs types --
-------------------------------------------------------------------


----------------- Nouvelle idée ----------------------

-- Cf les types dans Parsage.hs

-- Au départ, tout ce que l'on peut dire (au premier remplissage de l'arbre) est :
--      - Statement :: a
--      - Expr :: a
--      - BinOp :: a -> a -> a
--      - UnOp :: a -> a
--      - In :: Int
--      - Fl :: Float
--      - Boolean :: Bool
--      - Str :: String
--
-- Ensuite, on va pouvoir raffiner de plus en plus, tester la cohérence.
-- Plus tard, on pourra se mettre à inférer...




-- Maintenant on va propager nos types
-- et vérifier leur cohérence.
--
-- Pour un If expr s1 s2 t, il faut
--      - type(expr) = Bool
--      - type(s1) = type(s2)
--      - on en déduit alors le type du statement : t <- type(s1)
--
-- Pour un Let var val stmt
--      - pas encore supporté :-(


-- Construction de programme typé, avec potentiellement des types à Nothing si le typage
-- a échoué (conflits). Une autre fonction se chargera de l'affichage
-- TODO : transformer Maybe Type en Either (Type, Type) Type pourrait permettre de garder en
-- mémoire, en cas d'erreur, le type inféré ET le type attendu (messages d'erreur plus complets)



-- TODO : Vérifier qu'il n'y a pas de redéfinition de variable

class Typed a where
    typeof :: a -> Type
    infer_type :: a -> a
    -- utilisé pour propager le type d'une variable partout là où elle est visible
    propag_type :: String -> Type -> a -> a



instance Typed Statement where

    typeof (If _ _ _ t) = t
    typeof (Let _ _ _ t) = t
    typeof (Expr e t) = t


-- ATTENTION : les expressions sont typées mais pas substituées ! => seul le ain statement garde un type à la fin, pas bien !
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
    
    


-- TODO : mettre tout ça dans une monade IO pour afficher les erreurs en même temps ???

instance Typed Expr where

    typeof (Const _ t) = t
    typeof (Var _ t) = t
    typeof (Un _ _ t) = t
    typeof (Bin _ _ _ t) = t


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


    -- Les autres valeurs de type pour Var ne sont pas intéressantes :
    -- on a détecté les éventuelles redéfinitions de variables avant TODO
    propag_type var t (Var var' (Just [])) =
        if var == var' then
            Var var' t
        else
            Var var' (Just [])

    -- Pour les autres, il ne s'agit que de bêtes appels récursifs aussi...
    propag_type var t c@(Const _ _) = c

    propag_type var t (Un op e t_e) = let e' = propag_type var t e in Un op e' t_e

    propag_type var t (Bin op e1 e2 t_e) =
        let e1' = propag_type var t e1
            e2' = propag_type var t e2
        in
            Bin op e1' e2' t_e



main = let ast = parse stmt "" "let e = 4 in 10 + e"
        in
            case ast of 
                Left err -> putStrLn "Il y a eu une erreur de parse !"
                Right arbre -> let typed_arbre = infer_type arbre in do {
                                    {-putStrLn $ show arbre;-}
                                    putStrLn $ show typed_arbre;
                                }






-- Pas de gestion des redéfinitions de variable pour l'instant

-- On commente ce qui suit, car la substitution des Let n'est pas forcément la bonne solution.
-- On va commencer par gérer la propagation de types dans les expressions sans let, puis on 
-- ajoutera le support des expressions avec let, et on verra ce que ça donne


-- Ce qui suit n'est plus correct au niveau des types
{-elim_let :: Statement -> Statement-}
{-elim_let stmt =-}
    {-case stmt of-}
        {-Expr e -> Expr e-}
        {-If e s1 s2 -> If e (elim_let s1) (elim_let s2)-}
        {-Let var val s -> subs_stmt var val s-}


{-subs_stmt :: String -> Expr -> Statement -> Statement-}
{-subs_stmt var val stmt =-}
    {-case stmt of-}
        {-Expr e -> Expr $ subs_expr var val e-}
        {-If e s1 s2 -> If (subs_expr var val e) (subs_stmt var val s1) (subs_stmt var val s2)-}
        {-Let o_var o_val o_stmt ->-}
            {-let n_val = subs_expr var val o_val in -- substitution de la variable dans l'expression du let-}
                {-let n_stmt = subs_stmt o_var n_val o_stmt in -- élimitation du let le plus interne dans stmt-}
                    {-subs_stmt var val n_stmt -- fin de l'élimination du let courant dans le résultat précédent-}



{-subs_expr :: String -> Expr -> Expr -> Expr-}
{-subs_expr var val expr =-}
    {-case expr of-}
        {-Const con -> Const con-}
        {-Var str -> if str == var then -- on remplace les variables par leurs valeurs-}
                        {-val-}
                   {-else-}
                        {-Var str-}
        {-Bin binop e1 e2 -> Bin binop (subs_expr var val e1) (subs_expr var val e2)-}
        {-Un unop e -> Un unop (subs_expr var val e)-}
