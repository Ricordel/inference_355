module Typage where


import Parsage
import Text.ParserCombinators.Parsec

-- Pour commencer, afin de pouvoir n'utiliser que l'AST et ne pas garder une table des symboles
-- à côté (par souci de simplicité), on élimine les constructions de type let x = truc in machin
-- en remplaçant toutes les apparitions de "Var x" par l'expression "truc" dans le sous-arbre "machin"
-- Ce ne serait pas efficace pour de la vraie compilation, ce n'est pas non plus optimal en efficacité pour
-- du typage, mais ça me semble plus ismple à gérer dans un premier temps



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



{-main = let ast = parse stmt "" "let e = 3 in 19 + e + 5 + 6 + 7 -5"-}
main = let ast = parse stmt "" "1 + 2 - 3"
        in
            case ast of 
                Left err -> putStrLn "Il y a eu une erreur de parse !"
                Right arbre -> do {
                                    putStrLn $ show arbre;
                                }





-------------------------------------------------------------------
--  Maintenant, décorons not noeud et feuilles avec leurs types --
-------------------------------------------------------------------





-- Un niveau d'abstraction sur Statement, Expr, ... qui sont tous des noeuds, que l'on pourra décorer
-- de leurs types
{-data Node = Statement Statement | Expr Expr | Const Const-}
{-type TypedNode = (Node, Type)-}




-- Construction de l'arbre des types à partir de l'AST
-- On fait une descente récursive dans l'arbre :
--      - dans une première étape on initialise : typage des trucs connus, et Unknown pour les autres
--      - ensuite on raffine, en remontant depuis le bas

{-init_type :: Node -> Type-}
{-init_type node =-}
    {-case node of-}
        {-If cond e1 e2 -> Int -- pipo-}
        {-Expr e -> Float -- pipo-}










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


-- Une première chose triviale à inférer : le type des constantes : c'est le type de ce qu'il y a juste en dessous

-- Pour l'instant on ne traite que les arbres sans constructino Let

{-infer_const :: Statement -> Statement-}
{-infer_const stmt =-}
    {-case stmt of-}
        {-Expr (e, t) -> infer_const_e e-}
        {-If (e, te) (s1, t1), (s2, t2) -> If (infer_const e, te) (infer_const s1, t1) (infer_const s2, t2)-}


{-infer_const_e :: Expr -> Expr-}
{-infer_const_e expr =-}
    {-case expr of-}
        {-Const (con, t) -> case con of-}
                                {-In _ -> Const (con, Just [Int])-}
                                {-Fl _ -> Const (con, Just [Float])-}
                                {-Boolean _ -> Const (con, Just [Bool])-}
                                {-Str _ -> Const (con, Just [String])-}
        {-[>Var (str, t) -> -- On verra quand on implémentera le typage de let<]-}
        {-Un (op, t_op) (e, t_e) -> Un (op, t_op) (infer_const_e e, t_e) -- on infère juste les constantes, donc le type de e -}
        {-Bin (op, t_op) (e1, t_e1) (e2, t_e2) -> Bin (op, -}





















