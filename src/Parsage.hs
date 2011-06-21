-- Dans un premier temps, on va parser des expressions contenant des fonctions,
-- des opérateurs binaires infixes, un - préfixe, ...
-- Le résultat sera un arbre que l'on pourra décorer des types des expressions...


----------------------------------------------------------------------------------
----------------------------------------------------------------------------------
--                      Définition de la grammaire utilisée                     --
----------------------------------------------------------------------------------
----------------------------------------------------------------------------------

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




-- Des types pour représenter les types, un type inductif paraît pas mal
data SimpleType = Int | Float | Bool | String | Polym Char deriving Show
type Type = Maybe [SimpleType]
-- Just [] représente un terme "pas encore typé", typiquement lors de la création de l'AST
-- Nothing représente un typage qui a échoué


-- Les types de données utilisés dans notre AST

type Prog = [Statement]

data Statement = Expr (Expr, Type) | If (Expr, Type) (Statement, Type) (Statement, Type) | Let String (Expr, Type) (Statement, Type) deriving Show

data Expr = Const (Con, Type) | Var (String, Type) | Un (UnOp, Type) (Expr, Type) | Bin (BinOp, Type) (Expr, Type) (Expr, Type) deriving Show

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
            return (If (cond, Just []) (if_true, Just []) (if_false, Just []));
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
            return (Let ident (val, Just []) (st, Just []));
        }
        <|>
        do {
            e <- expr;
            return (Expr (e, Just []))
        }



-- La partie expressions se prète bien à l'utilisation de l'ExpressionParser fourni par Parsec
expr :: Parser Expr
expr = buildExpressionParser table factor



-- La table, qui contient les différents opérateurs par ordre de précédence
-- et indiquant leur associativité
--
-- TODO : transformer ça en arbre AVEC types
--
table :: OperatorTable Char () Expr
table = [
            [ unop "!" (mkNot), unop "-" (mkNegative) ]
            , [ unop "str2int" (mkStr2int), unop "str2float" (mkStr2float), unop "str2bool" (mkStr2bool) ]
            , [ unop "int2str" (mkInt2str), unop "float2str" (mkFloat2str), unop "bool2str" (mkBool2str) ]
            , [ binop "&&" (mkAnd) AssocLeft ]
            , [ binop "||" (mkOr) AssocLeft ]
            , [ binop ">" (mkSup) AssocNone, binop "<" (mkInf) AssocNone, binop ">=" (mkSupEq) AssocNone, binop "<=" (mkInfEq) AssocNone ]
            , [ binop "==" (mkEquals) AssocNone, binop "!=" (mkDiff) AssocNone ]
            , [ binop "*" (mkMult) AssocLeft, binop "/" (mkDiv) AssocLeft ]
            , [ binop "+" (mkPlus) AssocLeft, binop "-" (mkMinus) AssocLeft ]
            , [ binop "++" (mkConcat) AssocLeft ]
        ]
        where
            binop str fun assoc =
                Infix ( do { reservedOp str; return fun } <?> "Operator" ) assoc
                -- le <?> est là pour améliorer les messages d'erreur lors du parsage
            unop str fun = Prefix( do { reservedOp str; return fun } )

            mkNot e = Un (Not, Just [Bool, Bool]) (e, Just [])
            mkNegative e = Un (Negative, Just [Int, Int]) (e, Just [])
            mkStr2int e = Un (Str2int, Just [String, Int]) (e, Just [])
            mkStr2float e = Un (Str2float, Just [String, Float]) (e, Just [])
            mkStr2bool e = Un (Str2bool, Just [String, Bool]) (e, Just [])
            mkInt2str e = Un (Int2str, Just [Int, String]) (e, Just [])
            mkFloat2str e = Un (Float2str, Just [Float, String]) (e, Just [])
            mkBool2str e = Un (Bool2str, Just [Float, String]) (e, Just [])

            mkAnd e1 e2 = Bin (And, Just [Bool, Bool, Bool]) (e1, Just []) (e2, Just [])
            mkOr e1 e2 = Bin (Or, Just [Bool, Bool, Bool]) (e1, Just []) (e2, Just [])
            
            -- FIXME : doivent devenir polymorphe rapidement ! a -> a -> Bool
            mkEquals e1 e2 = Bin (Equals, Just [Int, Int, Bool]) (e1, Just []) (e2, Just [])
            mkDiff e1 e2 = Bin (Diff, Just [Int, Int, Bool]) (e1, Just []) (e2, Just [])

            -- FIXME : pareil si on choisi de comparer les chaînes
            mkSup e1 e2 = Bin (Sup, Just [Int, Int, Bool]) (e1, Just []) (e2, Just [])
            mkInf e1 e2 = Bin (Inf, Just [Int, Int, Bool]) (e1, Just []) (e2, Just [])
            mkSupEq e1 e2 = Bin (SupEq, Just [Int, Int, Bool]) (e1, Just []) (e2, Just [])
            mkInfEq e1 e2 = Bin (InfEq, Just [Int, Int, Bool]) (e1, Just []) (e2, Just [])

            -- FIXME : devrait devenir polymorphe aussi, ou alors on se la jour CamlLight
            -- avec +., -., ... pour les opérations sur les flottants
            mkMult e1 e2 = Bin (Mult, Just [Int, Int, Int]) (e1, Just []) (e2, Just [])
            mkDiv e1 e2 = Bin (Div, Just [Int, Int, Int]) (e1, Just []) (e2, Just [])
            mkPlus e1 e2 = Bin (Plus, Just [Int, Int, Int]) (e1, Just []) (e2, Just [])
            mkMinus e1 e2 = Bin (Minus, Just [Int, Int, Int]) (e1, Just []) (e2, Just [])

            mkConcat e1 e2 = Bin (Concat, Just [String, String, String]) (e1, Just []) (e2, Just [])



factor = parens expr
         <|> 
         do { iden <- identifier;
              return $ Var (iden, Just [])
         }
         <|>
         do { i <- integer;
              return $ Const ((In i), Just [Int])
         }
         <|> 
         do { x <- float;
              return $ Const ((Fl x), Just [Float])
         }
         <|> 
         do { reserved "True";
              return $ Const ((Boolean True), Just [Bool])
         }
         <|>
         do { reserved "False";
              return $ Const ((Boolean False), Just [Bool])
         }
         <|>
         do { str <- stringLiteral;
             return $ Const ((Str str), Just [String])
         }
