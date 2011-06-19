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
{-const ::= Int | Float | Bool-}
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

data Con = In Integer | Fl Double | Boolean Bool deriving Show

data UnOp = Not | Negative deriving Show

data BinOp = Plus | Minus | Mult | Div | And | Or | Equals | Diff | Inf | Sup | InfEq | SupEq deriving Show




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
              reservedOpNames = ["+", "-", "*", "/", "<", ">", ">=", "<=", "&&", "||", "==", "!="]
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
            [ op "&&" (Bin And) AssocLeft ]
            , [ op "||" (Bin Or) AssocLeft ]
            , [ op ">" (Bin Sup) AssocNone, op "<" (Bin Inf) AssocNone, op ">=" (Bin SupEq) AssocNone, op "<=" (Bin InfEq) AssocNone ]
            , [ op "==" (Bin Equals) AssocNone, op "!=" (Bin Diff) AssocNone ]
            , [ op "*" (Bin Mult) AssocLeft, op "/" (Bin Div) AssocLeft ]
            , [ op "+" (Bin Plus) AssocLeft, op "-" (Bin Minus) AssocLeft ]
        ]
        where
            op str fun assoc =
                Infix ( do { reservedOp str; return fun } <?> "Operator" ) assoc
                -- le <?> est là pour améliorer les messages d'erreur lors du parsage


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



