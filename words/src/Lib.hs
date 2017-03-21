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
    ,genCell2
    ,genCellMonad
    ,rows
    ,cols
    ,rows8
    ,cols8
    ,repeat8
    ,fmt
    ,myzip
    ,zipDemo
    ,zipWithDemo
    ,zipOverGrid
    ,coordInf
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

fmt :: Show a => [a] -> IO()
fmt  = putStrLn .unlines . map show
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

--cannot generater the right cell  : compare to function genCellMonad
--should use loop
--this is a wrong idea and can nerver get the same function like genCellMonad,for this
--1. rows <- replicate 10 [0..10]
--2. cols <- [0.10]
--and the construction of rows is [[]] and the cols is [] ,so you have to make them the
--same construction like [[]]
genCell = map (zip [0..14]) (map (replicate 10) [0..11])

--generater cell in a list
genCell2 = do
    rows <- [1..10]
    cols <- [1..10]
    return (rows, cols)

--generator cellList, use loop in haskell
genCellMonad = do
    rows <- [0..10]
    return $ do
      cols <- [0..10]
      return (rows,cols)

cols = repeat [0..]
rows = map repeat [0..]
coordInf = zipOverGrid rows cols

repeat8 = take 8 . repeat
cols8 = repeat8 [0..7]
rows8 = map repeat8 [0..7]

--:t zipOverGrid  : to check the type of two param and  return type
zipOverGrid = zipWith zip

--like the name zipWith is zip every element with a function
--and zip is zip every element of each list and make them into a tuple
myzip = (,)  1 2
zipDemo = zip [1,1,1] [2,2,2]
zipWithDemo = zipWith zip [[1,1,1]] [[2,2,2]]



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
