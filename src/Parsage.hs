-- Dans un premier temps, on va parser des expressions contenant des fonctions,
-- des opérateurs binaires infixes, un - préfixe, ...
-- Le résultat sera un arbre que l'on pourra décorer des types des expressions...


----------------------------------------------------------------------------------
----------------------------------------------------------------------------------
--                      Définition de la grammaire utilisée                     --
----------------------------------------------------------------------------------
----------------------------------------------------------------------------------

-- Attention, pour que Parsec soit vraiment pratique efficace, il vaut mieux que la
-- grammaire soit LL1.
{-prog ::= statement { ; statement }*-}

{-statement ::= if expr then statement else statement-}
            {-| let var = expr in statement-}

{-expr ::= const | var | (expr) | unOp expr | expr binOp expr-}
{-const ::= Int | Float | Bool | String -}
{-unOp ::= - | !-}
{-binOp ::= + | - | * | / | && | "||" | == | != | < | > | <= | >=-}
{-Int ::= digit+-}
{-Float ::= Int '.' Int-}
{-Bool ::= "True" | "False"-}

-- Haskell nous fourni Parsec, ne nous ne privons pas !

module Parsage where

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import qualified Text.ParserCombinators.Parsec.Token as PT
import Text.ParserCombinators.Parsec.Language




-- Les types de données utilisés dans notre AST

type Prog = [Statement]

data Statement = Expr Expr | If Expr Statement Statement | Let String Expr Statement deriving Show

data Expr = Const Con | Var String | Un UnOp Expr | Bin BinOp Expr Expr deriving Show

data Con = In Integer | Fl Double | Boolean Bool | Str String deriving Show

-- pour l'instant les fonctions sont unaires et hard-codées, c'est amené à évoluer !
data UnOp = Not | Negative | Int2str | Float2str | Bool2str | Str2int | Str2float | Str2bool deriving Show

data BinOp = Plus | Minus | Mult | Div | And | Or | Equals | Diff | Inf | Sup | InfEq | SupEq | Concat deriving Show




-- Le style du langage :
--      les commentaires seront "à la C"
--      la déclaration des mots réservés évite les problèmes si un nom de variable commence
--      par la même chose qu'un mot réservé (e.g une variable nommée "lettre" poserait problème à cause de "let")
myStyle :: LanguageDef ()
myStyle = emptyDef
            { commentStart = "/*" ,
              commentEnd = "*/" ,
              commentLine = "//" ,
              reservedNames = ["else", "False", "if", "in", "let", "then", "True", "="] ,
              reservedOpNames = ["+", "-", "*", "/", "<", ">", ">=", "<=", "&&", "||", "==", "!=", "++"]
            }


-- Le lexer qui créera le flux de tokens
lexer :: PT.TokenParser ()
lexer = PT.makeTokenParser myStyle
        

-- Ceci nous fourni alors divers parseurs (pour les expaces blancs, les entiers, les
-- expressions parenthèsées, les mots réservés, ...)
-- Pour plus de lisibilité dans la suite, on bind directement ici ceux qui nous seront utiles

whiteSpace = PT.whiteSpace lexer -- pour les espaces
symbol = PT.symbol lexer -- string s ET ignore les espaces finaux
integer = PT.integer lexer -- un élément de Z
hexadecimal = PT.hexadecimal lexer -- Parce qu'on aime faire des calculs en hexa, doit commencer par 0x ou 0X
float = PT.float lexer -- un flottant
parens = PT.parens lexer -- pour un élément entre parenthèses
semiSep1 = PT.semiSep1 lexer -- séquence d'expressions séparées par des ";"
identifier = PT.identifier lexer -- 
reserved = PT.reserved lexer
reservedOp = PT.reservedOp lexer
stringLiteral = PT.stringLiteral lexer


------ Le parseur -------
-- On doit retourner un AST de notre programme, donc le parseur sera de type Parser Statement

-- Parser pour le programme complet : plusieurs statements séparés par des ";"
prog :: Parser Prog
prog = semiSep1 stmt


-- Un statement : if then else | let in
stmt :: Parser Statement
stmt = do { 
            -- if cond then if_true else if_false
            reserved "if";
            cond <- expr;
            reserved "then";
            if_true <- stmt;
            reserved "else";
            if_false <- stmt;
            return (If cond if_true if_false);
          }
            <|>
        do {
            -- let ident = val in st
            reserved "let";
            ident <- identifier;
            reserved "=";
            val <- expr;
            reserved "in";
            st <- stmt;
            return (Let ident val st);
        }
        <|>
        do {
            e <- expr;
            return (Expr e)
        }



-- La partie expressions se prète bien à l'utilisation de l'ExpressionParser fourni par Parsec
expr :: Parser Expr
expr = buildExpressionParser table factor



-- La table, qui contient les différents opérateurs par ordre de précédence
-- et indiquant leur associativité
--
-- !!! Pour l'instant il n'y a pas les opérateurs unaires => voir du côté des Operator
table :: OperatorTable Char () Expr
table = [
            [ unop "!" (Un Not), unop "-" (Un Negative) ]
            , [ unop "str2int" (Un Str2int), unop "str2float" (Un Str2float), unop "str2bool" (Un Str2bool) ]
            , [ unop "int2str" (Un Int2str), unop "float2str" (Un Float2str), unop "bool2str" (Un Bool2str) ]
            , [ binop "&&" (Bin And) AssocLeft ]
            , [ binop "||" (Bin Or) AssocLeft ]
            , [ binop ">" (Bin Sup) AssocNone, binop "<" (Bin Inf) AssocNone, binop ">=" (Bin SupEq) AssocNone, binop "<=" (Bin InfEq) AssocNone ]
            , [ binop "==" (Bin Equals) AssocNone, binop "!=" (Bin Diff) AssocNone ]
            , [ binop "*" (Bin Mult) AssocLeft, binop "/" (Bin Div) AssocLeft ]
            , [ binop "+" (Bin Plus) AssocLeft, binop "-" (Bin Minus) AssocLeft ]
            , [ binop "++" (Bin Concat) AssocLeft ]
        ]
        where
            binop str fun assoc =
                Infix ( do { reservedOp str; return fun } <?> "Operator" ) assoc
                -- le <?> est là pour améliorer les messages d'erreur lors du parsage
            unop str fun = Prefix( do { reservedOp str; return fun } )

factor = parens expr
         <|> 
         do { iden <- identifier;
              return $ Var iden
         }
         <|>
         do { i <- integer;
              return $ Const (In i)
         }
         <|> 
         do { x <- float;
              return $ Const (Fl x)
         }
         <|> 
         do { reserved "True";
              return $ Const (Boolean True)
         }
         <|>
         do { reserved "False";
              return $ Const (Boolean False)
         }
         <|>
         do { str <- stringLiteral;
             return $ Const (Str str)
         }





-------------------------------------
-- Problème de chargement de modules dans le REPL, on verra plus tard => le code de typage se retrouve ici



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



main = let ast = parse stmt "" "let e = 3 in 19 + e"
        in
            case ast of 
                Left err -> putStrLn "Il y a eu une erreur de parse !"
                Right arbre -> let simplified = elim_let arbre in
                                    do {
                                        putStrLn $ show arbre;
                                        putStrLn $ show simplified
                                    }
