module Lib
    ( someFunc
    ,someString
    ,grid
    ,languages
    ,getLines
    ,formatGrid
    ,isWordInLine
    ,findWords
    ,findWord
    ) where
import Data.List(isInfixOf,reverse)
import Data.Maybe(catMaybes)

someString :: String
someString = "someString"

someFunc :: IO ()
someFunc = putStrLn "someFunc"

type Grid = [String]

getLines = unlines

formatGrid :: Grid -> IO()
formatGrid grid = putStrLn $ getLines grid


findWords :: Grid -> String -> Maybe String
findWords grid  word =
  let lines = grid ++ map reverse grid
      found =  or $ map (isWordInLine word) lines
  in if found then Just word else Nothing

findWord grid words =
  let found = map (findWords grid) languages
  in catMaybes found


isWordInLine :: String -> String -> Bool
isWordInLine = isInfixOf

grid = [ "__C________R___"
       , "__SI________U__"
       , "__HASKELL____B_"
       , "__A__A_____S__Y"
       , "__R___B___C____"
       , "__PHP____H_____"
       , "____S_LREP_____"
       , "____I__M_Y__L__"
       , "____L_E__T_O___"
       , "_________HB____"
       , "_________O_____"
       , "________CN_____"
       ]

languages = ["BASIC"
            , "COBOL"
            , "CSHARP"
            , "HASKELL"
            , "LISP"
            , "PERL"
            , "PHP"
            , "PYTHON"
            , "RUBY"
            , "SCHEME"
            ]
