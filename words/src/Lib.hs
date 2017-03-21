module Lib
    ( someFunc
    ,someString
    ,grid
    ,languages
    ,getLines
    ,formatGrid
    ,isWordInLine
    ,isWordInGrid
    ,isWordInLine2
    ,isWordInGrid2
    ,findWords
    ,findWordsInGrid
    ,generatorCell
    ,genCell
    ,mulit2
    ,filtered
    ,filtered2
    ,composeValue
    ,lambadaCompose
    ) where
import Data.List(isInfixOf,reverse)
import Data.Maybe(catMaybes)
import Control.Monad(guard)

someString :: String
someString = "someString"

someFunc :: IO ()
someFunc = putStrLn "someFunc"

type Grid = [String]

getLines = unlines

formatGrid :: Grid -> IO()
formatGrid grid = putStrLn $ getLines grid

{-


-}

isWordInLine :: String -> String -> Bool
isWordInLine = isInfixOf

isWordInGrid :: Grid -> String -> [Bool]
isWordInGrid grid word =
    let lines = grid
        found = map (isWordInLine word) lines
    in  found
{-


-}
isWordInLine2 :: String -> String -> Maybe String
isWordInLine2 word line =
      let found = isInfixOf word line
      in if found then Just word else Nothing

isWordInGrid2 :: Grid -> String -> [Maybe String]
isWordInGrid2 grid word =
      let lines = grid
          found = map (isWordInLine2 word) lines
      in found

{-


-}
findWords :: Grid -> String -> String
findWords grid word =
      let lines = grid
          found = catMaybes $ isWordInGrid2 lines word
       in if found == [] then "" else head found

findWordsInGrid :: Grid ->[String] -> [String]
findWordsInGrid grid words =
      let lines = grid
          langs = languages
          found = map (findWords lines) langs
       in filter (/= "") found

generatorCell =
  zip (replicate 12 0) [0..14] :
  zip (replicate 12 1) [0..14] :
  []


{-
  monad test
-}

mulit2 = do
  i <- [0..10]
  return (i * 2)

div2 x = x `mod` 2 == 0

filtered = do
  i <- [0..]
  guard (div2 i)
  return i

filtered2 = [ i * 2 | i <-[0..10], div2 i]

composeValue = map (take 10 . repeat) [0..10]
lambadaCompose = map (\x-> take 10 $ repeat x) [10..20]
genCell = map (zip [0..14]) (map (replicate 10) [0..11])
--genCell = zip xx [0..14]

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
