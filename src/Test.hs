module Test where

import Typage
import Parsage
import Text.ParserCombinators.Parsec

-- Quelques tests

-- La génération de tests aléatoires avec QuickCheck est un peu lourde dans le cas de codes source,
-- je me contenterai de quelques tests à la main...

check :: String -> Type -> IO ()
check s t = 
    let rslt = parse stmt "" s
    in
        case rslt of
            Left err -> print err
            Right ast -> if (typeof $ infer_type ast) == t then print "Succes" else print ("Error : " ++ s)



main = do {
        check "4 == 3" (Just [Bool]);
        check "let x = 4 in x + 2" (Just [Int]);
        check "if 3 == 5 then \"foo\" else \"bar\"" (Just [String]);
        check "if 3 == \"foo\" then 4 else 5" Nothing;
        check "let f = fun(x, y, z) : if x == y then z else 0 in 5 * f(1, 2, 3)" (Just [Int]);
        check "let g = fun(a, str) : let x = 3 in if x > a then str else bool2str True in g(4, \"coucou\")" (Just [String]);
}
