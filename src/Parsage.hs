-- Dans un premier temps, on va parser des expressions contenant des fonctions,
-- des opérateurs binaires infixes, un - préfixe, ...
-- Le résultat sera un arbre que l'on pourra décorer des types des expressions...

------------------------------------------------------------------------------------
-- La première étape consiste en le parsage des expressions que nous allons
-- utiliser. Elles resteront très simple, seules seront implémentées les expressions
-- du style
--      - if cond then action_si_true else action_si_false
--      - let var = expression in action
--      - les expressions arithmétiques classiques plus quelques autres fonctions
--        et opérateurs built-in comme ++ (concat), str2int, ...
------------------------------------------------------------------------------------



-- Plus formellement, voici ça grammaire que l'on utilise (pour l'instant)

{-
  statement ::= if expr then statement else statement
            | let var = expr in statement
            | expr
 
 expr ::= const | var | (expr) | unOp expr | expr binOp expr
 const ::= Int | Float | Bool | String
 unOp ::= - | ! | <autres fonctions built-in>
 binOp ::= + | - | * | / | && | "||" | == | != | < | > | <= | >= | ++
 Int ::= digit+
 Float ::= Int '.' Int
 Bool ::= "True" | "False"
 -}


-- Pour le parsage des expression, nous utiliserons la bibliothèque Haskell par défaut
-- Parsec, qui offre de nombreux outils pour réaliser ce genre de tâches

module Parsage where

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import qualified Text.ParserCombinators.Parsec.Token as PT
import Text.ParserCombinators.Parsec.Language


-----------------------------------------
-- Les types utiles dans ce module
-----------------------------------------


-- Même si on ne s'occupe pas de typage ici, on décore l'arbre avec des types vides (et
-- déjà les types définitifs pour les constantes), on a donc besoin d'une visibilité sur
-- les types utilisés pour représenter les types (...)
--

-- Nos types de base sont réduits à des choses simples, Polym est prévu
-- pour pouvoir gérer des fonctions polymorphes (non utilisé pour l'instant)

-- TODO : ajouter des listes serait pas mal
data SimpleType = Int | Float | Bool | String | Polym Char deriving (Eq, Show)


-- Un type à proprement parler est représenté par une liste de types simples
-- la liste [a, b, c] représente le type a -> b -> c
-- On encapsule le tout dans une monade Maybe :
--      - un type Nothing dénote l'échec du processus de typage
--      - Just [qqch] un type qui a été calculé
--      - Just [] est le type donné aux expressions dont le type n'a pas encore été calculé
--
-- TODO : ça fait beaucoup de Just partout dans le code de Typage, essayer de trouver mieux...
type Type = Maybe [SimpleType]



-- Les types de données correspondant à la grammaire donnée plus tôt

data Statement = Expr Expr Type
               | If Expr Statement Statement Type
               | Let String Statement Statement Type
               deriving Show


data Expr = Const Con Type
            | Var String Type
            | Un (UnOp, Type) Expr Type
            | Bin (BinOp, Type) Expr Expr Type
            | FunCall (String, Type) [Expr] Type -- le premier Type est celui de la fonction, le 2è celui du résultat de son application aux arguments
            | FunDef [String] Statement Type -- args, corps de la lambda
            deriving Show


data Con = In Integer | Fl Double | Boolean Bool | Str String deriving Show

-- TODO : pour l'instant les fonctions sont unaires et hard-codées, prochaîne étape :
-- TODO gérer la déclaration de fonctions
data UnOp = Not | Negative | Int2str | Float2str | Bool2str | Str2int | Str2float | Str2bool deriving Show

data BinOp = Plus | Minus | Mult | Div | And | Or | Equals | Diff | Inf | Sup | InfEq | SupEq | Concat deriving Show







-------------------------------------------------------
-- Le parseur en lui-même, avec utilisation de Parsec
-------------------------------------------------------


-- Style du langage
myStyle :: LanguageDef ()
myStyle = emptyDef
            { commentStart = "/*" , -- commentaires façon C
              commentEnd = "*/" ,
              commentLine = "//" ,
              reservedNames = ["else", "False", "fun", "if", "in", "let", "then", "True", "="] ,
              reservedOpNames = ["+", "-", "*", "/", "<", ">", ">=", "<=", "&&", "||", "==", "!=", "++"]
            }


-- La création du lexer
lexer :: PT.TokenParser ()
lexer = PT.makeTokenParser myStyle
        

-- Ceci nous fourni alors divers parseurs (pour les expaces blancs, les entiers, les
-- expressions parenthésées, les mots réservés, ...)
-- Pour plus de lisibilité dans la suite, on bind directement ici ceux qui nous seront utiles

whiteSpace = PT.whiteSpace lexer -- pour les espaces
symbol = PT.symbol lexer
integer = PT.integer lexer -- un élément de Z
float = PT.float lexer -- un flottant
parens = PT.parens lexer -- pour un élément entre parenthèses
identifier = PT.identifier lexer -- pour les noms de variable
reserved = PT.reserved lexer -- pour un mot réservé
reservedOp = PT.reservedOp lexer -- pour un opérateur réservé
stringLiteral = PT.stringLiteral lexer -- Pour les chaînes
commaSep1 = PT.commaSep1 lexer -- pour la séparation des arguments de fonction




------ Maintenant qu'on a le lexer, continuons avec le parseur -------

-- Un programme entier est un statement.
-- Pour le toplevel, nous devons gérer "à la main" (mais on est bien aidés quand
-- même) les espaces blancs du départ. On vérifie aussi la présence de eof afin
-- qu'une parenthèse fermante de trop ne nous fasse pas croire qu'on a terminé
-- de parses avec succès alors qu'on s'est en fait arrêté à la moitié du programme...
prog :: Parser Statement
prog = do 
            whiteSpace
            s <- stmt
            eof
            return s


-- 3 formes possibles pour le statement
stmt :: Parser Statement
stmt = 
        parens stmt
        <|>
        do { 
            -- if cond then if_true else if_false
            reserved "if";
            cond <- expr;
            reserved "then";
            if_true <- stmt;
            reserved "else";
            if_false <- stmt;
            return (If cond if_true if_false (Just []));
          }
            <|>
        do {
            -- let ident = val in st
            reserved "let";
            ident <- identifier;
            reserved "=";
            val <- stmt;
            reserved "in";
            {-symbol "{";-}
            st <- stmt;
            {-symbol "}";-}
            return (Let ident val st (Just []));
        }
        <|>
        do {
            e <- expr;
            return (Expr e (Just []))
        }



-- En ce qui concerne les expressions, Parsec fourni un utilitaire très pratique pour parser ce genre
-- de choses. Il suffit de lui fournir la liste de nos opérateurs et fonctions built-in, et il se débrouille.
-- TODO : il serait bien de déclarer nos opérateurs et fonctions built-in dans un fichier séparé plutôt que
-- directement dans le code source, on verra plus tard
expr :: Parser Expr
expr = buildExpressionParser table factor


-- Les différents opérateurs sont donnée du plus prioritaire au moins prioritaire, ceux sur la même
-- ligne ont même priorité.
-- cf doc de Parsec pour plus de détails
table :: OperatorTable Char () Expr
table = [
            [ unop "!" (mkNot), unop "-" (mkNegative) ]
            , [ unop "str2int" (mkStr2int), unop "str2float" (mkStr2float), unop "str2bool" (mkStr2bool) ]
            , [ unop "int2str" (mkInt2str), unop "float2str" (mkFloat2str), unop "bool2str" (mkBool2str) ]
            , [ binop "*" (mkMult) AssocLeft, binop "/" (mkDiv) AssocLeft ]
            , [ binop "+" (mkPlus) AssocLeft, binop "-" (mkMinus) AssocLeft ]
            , [ binop "++" (mkConcat) AssocLeft ]
            , [ binop "==" (mkEquals) AssocNone, binop "!=" (mkDiff) AssocNone ]
            , [ binop ">" (mkSup) AssocNone, binop "<" (mkInf) AssocNone, binop ">=" (mkSupEq) AssocNone, binop "<=" (mkInfEq) AssocNone ]
            , [ binop "&&" (mkAnd) AssocLeft ]
            , [ binop "||" (mkOr) AssocLeft ]
        ]
        where
            binop str fun assoc =
                Infix ( do { reservedOp str; return fun } <?> "Operator" ) assoc
                -- le <?> est là pour améliorer les messages d'erreur lors du parsage
            unop str fun = Prefix( do { reservedOp str; return fun } <?> "Unary Operator" )

-- FIXME  ceci est le code le moins modulaire et le moins factorisé du monde,
-- FIXME  ça serait bien de trouver une solution moins moche
--
            mkNot e       = Un (Not, Just [Bool, Bool])          e (Just [])
            mkNegative e  = Un (Negative, Just [Int, Int])       e (Just [])
            mkStr2int e   = Un (Str2int, Just [String, Int])     e (Just [])
            mkStr2float e = Un (Str2float, Just [String, Float]) e (Just [])
            mkStr2bool e  = Un (Str2bool, Just [String, Bool])   e (Just [])
            mkInt2str e   = Un (Int2str, Just [Int, String])     e (Just [])
            mkFloat2str e = Un (Float2str, Just [Float, String]) e (Just [])
            mkBool2str e  = Un (Bool2str, Just [Bool, String])  e (Just [])

            mkAnd e1 e2 = Bin (And, Just [Bool, Bool, Bool]) e1 e2 (Just [])
            mkOr e1 e2  = Bin (Or, Just [Bool, Bool, Bool])  e1 e2 (Just [])
            
            -- FIXME : doivent devenir polymorphe a -> a -> Bool
            mkEquals e1 e2 = Bin (Equals, Just [Int, Int, Bool]) e1 e2 (Just [])
            mkDiff e1 e2   = Bin (Diff, Just [Int, Int, Bool])   e1 e2 (Just [])

            -- FIXME : pareil si on choisi de comparer les chaînes (sinon ajouter
            -- des opérateurs supplémentaires pour les Floats...)
            mkSup e1 e2   = Bin (Sup, Just [Int, Int, Bool])   e1 e2 (Just [])
            mkInf e1 e2   = Bin (Inf, Just [Int, Int, Bool])   e1 e2 (Just [])
            mkSupEq e1 e2 = Bin (SupEq, Just [Int, Int, Bool]) e1 e2 (Just [])
            mkInfEq e1 e2 = Bin (InfEq, Just [Int, Int, Bool]) e1 e2 (Just [])

            -- FIXME : devrait devenir polymorphe aussi, ou alors on se la jour CamlLight
            -- avec +., -., ... pour les opérations sur les flottants (car comment définir
            -- / pour des chaînes ? Et pas envie de gérer des classes, j'en suis loin)
            mkMult e1 e2  = Bin (Mult, Just [Int, Int, Int])  e1 e2 (Just [])
            mkDiv e1 e2   = Bin (Div, Just [Int, Int, Int])   e1 e2 (Just [])
            mkPlus e1 e2  = Bin (Plus, Just [Int, Int, Int])  e1 e2 (Just [])
            mkMinus e1 e2 = Bin (Minus, Just [Int, Int, Int]) e1 e2 (Just [])

            mkConcat e1 e2 = Bin (Concat, Just [String, String, String]) e1 e2 (Just [])



-- le facteur de base à donner au constructeur de parseur d'expressions
factor = parens expr
         <|>
         do {
             iden <- identifier;
             do {
                 try (symbol "(");
                 args <- commaSep1 expr;
                 symbol ")";
                 return $ FunCall (iden, Just []) args (Just [])
             }
             <|>
             do {
                 return $ Var iden (Just [])
             }

         }
         <|> -- Définition de fonction dans un let ... in ...
         do { reserved "fun";
              args <- parens $ commaSep1 identifier;
              symbol ":";
              def <- stmt;
              return $ FunDef args def (Just [])
         } 
         <|>
         do { i <- integer;
              return $ Const (In i) (Just [Int])
         }
         <|> 
         do { x <- float;
              return $ Const (Fl x) (Just [Float])
         }
         <|> 
         do { reserved "True";
              return $ Const (Boolean True) (Just [Bool])
         }
         <|>
         do { reserved "False";
              return $ Const (Boolean False) (Just [Bool])
         }
         <|>
         do { str <- stringLiteral;
             return $ Const (Str str) (Just [String])
         }

