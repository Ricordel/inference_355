module Typage where


import Parsage
import Text.ParserCombinators.Parsec

-- Pour commencer, afin de pouvoir n'utiliser que l'AST et ne pas garder une table des symboles
-- à côté (par souci de simplicité), on élimine les constructions de type let x = truc in machin
-- en remplaçant toutes les apparitions de "Var x" par l'expression "truc" dans le sous-arbre "machin"
-- Ce ne serait pas efficace pour de la vraie compilation, ce n'est pas non plus optimal en efficacité pour
-- du typage, mais ça me semble plus ismple à gérer dans un premier temps



-- Pas de gestion des redéfinitions de variable pour l'instant



elim_let :: Statement -> Statement
elim_let stmt =
    case stmt of
        Expr e -> Expr e
        If e s1 s2 -> If e (elim_let s1) (elim_let s2)
        Let var val s -> subs_stmt var val s


subs_stmt :: String -> Expr -> Statement -> Statement
subs_stmt var val stmt =
    case stmt of
        Expr e -> Expr $ subs_expr var val e
        If e s1 s2 -> If (subs_expr var val e) (subs_stmt var val s1) (subs_stmt var val s2)
        Let o_var o_val o_stmt ->
            let n_val = subs_expr var val o_val in -- substitution de la variable dans l'expression du let
                let n_stmt = subs_stmt o_var n_val o_stmt in -- élimitation du let le plus interne dans stmt
                    subs_stmt var val n_stmt -- fin de l'élimination du let courant dans le résultat précédent




subs_expr :: String -> Expr -> Expr -> Expr
subs_expr var val expr =
    case expr of
        Const con -> Const con
        Var str -> if str == var then -- on remplace les variables par leurs valeurs
                        val
                   else
                        Var str
        Bin binop e1 e2 -> Bin binop (subs_expr var val e1) (subs_expr var val e2)
        Un unop e -> Un unop (subs_expr var val e)



{-main = let ast = parse stmt "" "let e = 3 in 19 + e + 5 + 6 + 7 -5"-}
main = let ast = parse stmt "" "1 + 2 - 3"
        in
            case ast of 
                Left err -> putStrLn "Il y a eu une erreur de parse !"
                Right arbre -> let simplified = elim_let arbre in
                                    do {
                                        putStrLn $ show arbre;
                                        putStrLn $ show simplified
                                    }





-------------------------------------------------------------------
--  Maintenant, décorons not noeud et feuilles avec leurs types --
-------------------------------------------------------------------


-- Des types pour représenter les types, un type inductif paraît pas mal
data Simple_t = Int | Float | Bool | String
data Type = Undefined | Simple Simple_t | Functional Simple_t Type -- devrait permettre de gérer les types fonctionels plus tard


-- Un niveau d'abstraction sur Statement, Expr, ... qui sont tous des noeuds, que l'on pourra décorer
-- de leurs types
data Node = Statement | Expr | Con | BinOp | UnOp
type TypedNode = (Node, Type)




-- Construction de l'arbre des types à partir de l'AST
typed_tree :: Statement -> TypedNode
typed_tree stmt =
    case stmt of
        Expr e -> 
