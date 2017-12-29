{-# LANGUAGE NamedFieldPuns, ScopedTypeVariables, LambdaCase, MultiWayIf #-}
module Sol21
    (run
    ) where

import Data.List.Split (splitOn, chunksOf)
import Data.List (partition, find, transpose, nub)
import Data.Maybe (fromMaybe)
import qualified Data.Map as M
import Debug.Trace (trace)

seed =
  [".#."
  ,"..#"
  ,"###"
  ]

rotateLeft :: [[a]] -> [[a]]
rotateLeft = reverse . transpose

flipsAndRots :: [String] -> [[String]]
flipsAndRots rs = nub (take 4 (iterate rotateLeft rs)) ++ (take 4 (iterate rotateLeft (map reverse rs)) )

ruleMatch :: [String] -> [String] -> Bool
ruleMatch xs rs = any (\r -> r == xs) $ flipsAndRots rs

parseLine :: String -> ([String], [String])
parseLine rStr = let
  rM:rR:[] = map (splitOn "/") $ splitOn " => " rStr
  in (rM, rR)

type Rule = ([String], [String])
type Rules = M.Map Int [Rule]

parseInput :: [String] -> Rules
parseInput ruleStr = let
  rs = map parseLine ruleStr
  f23 = (\r -> (length (fst r)) == 2)
  (twos, threes) =  partition f23 rs
  in M.fromList [(2, twos), (3, threes)]

-- > subdivide 2 ["0011", "0011", "2233", "2233"]
-- [["00","00"],["11","11"],["22","22"],["33","33"]]
subdivide :: Int -> [String] -> (Int, [[String]])
subdivide n grid = let
  zf = case n of
         2 -> \(a:b:[]) -> zipWith (\a b -> [a, b]) a b
         3 -> \(a:b:c:[]) -> zipWith3 (\a b c -> [a, b, c]) a b c
  divided = chunksOf n (map (chunksOf n) grid)
  in ((length grid) `div` n, concatMap zf $ divided)

-- > recombine [["00","00"],["11","11"],["22","22"],["33","33"]]
-- ["0011","0011","2233","2233"]
recombine :: Int -> [[String]] -> [String]
recombine perRow chunks = let
  rows = chunksOf perRow chunks in
  concatMap (\r -> foldl1 (\acc xs -> zipWith (++) acc xs) r) rows

applyRules :: [Rule] -> [String] -> [String]
applyRules rs x = let
  match = find (ruleMatch x . fst) rs
  Just (rule, result) = match
  in result

enhance :: Rules -> [String] -> [String]
enhance rules grid = let
  n = if (length grid `rem` 2 == 0) then 2 else 3
  rs = fromMaybe [] (M.lookup n rules)
  (c, subd) = subdivide n grid
  subd' = (map (applyRules rs) subd)
  in recombine c subd'

exampleRules = parseInput . lines $ "\
\../.# => ##./#../...\n\
\.#./..#/### => #..#/..../..../#..#"

example = let
  result = last $ take 3 $ iterate (enhance exampleRules) seed
  in (length $ filter (\x -> x == '#') $ concat result, result)

stillOnAfter :: Int -> Rules -> (Int, [String])
stillOnAfter n rules = let
  results = take (n+1) $ iterate (enhance rules) seed
  result = last $ results
  on = length $ filter (\x -> x == '#') $ concat result
  in (on, result)

printGrid :: [String] -> IO ()
printGrid g = do
  mapM_ putStrLn g
  putStrLn ""

run :: IO ()
run = do
  rulesRaw <- readFile "data/21.txt"
  let rules = parseInput . lines $ rulesRaw
  let (on, result) = stillOnAfter 5 rules
  printGrid result
  putStrLn (show on)
  let (on, result) = stillOnAfter 18 rules
  -- printGrid result
  putStrLn (show on)
