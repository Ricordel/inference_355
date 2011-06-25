module Main where

import Typage
import Parsage
import Text.ParserCombinators.Parsec
import Data.List
import System.Environment


-- Affiche la liste des fonctions avec leurs types, ainsi que le type total du programme

format_output :: Statement -> String
format_output s =
    format_statement s ++ "\n Type global du programme : " ++ (format_type $ typeof s)

format_statement :: Statement -> String
format_statement (If cond s1 s2 t) =
    format_statement s1 ++ "\n" ++ format_statement s2

format_statement (Let f (Expr (FunDef _ def t) _) s t_let) =
    format_statement def ++ format_statement s ++ "\n" ++ "Fonction : " ++ f ++ " (" ++ format_type t ++ ")\n"

format_statement _ = ""



-- formate un type pour passer de [t, t', t'', ...] à la notation plus habituelle
-- t -> t' -> t'' -> ...
format_type :: Type -> String
format_type Nothing = "Enorme échec ! (le typage a échoué)"
format_type (Just []) = " gros échec ! (n'a pas été typé...)"
format_type (Just t) =
    let str_t = map show t
    in
        concat $ intersperse " -> " str_t




main = do
        file_name : other_args <- getArgs
        rslt <- parseFromFile stmt file_name
        case rslt of
            Left err -> print err
            Right ast -> (putStrLn (show $ infer_type ast)) >> (putStrLn "\n\n") >> (putStrLn $ format_output $ infer_type ast)
