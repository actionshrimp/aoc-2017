{-# LANGUAGE NamedFieldPuns, ScopedTypeVariables, LambdaCase, MultiWayIf #-}
module Sol21
    (run
    ) where

import Data.List.Split (splitOn, chunksOf)
import Data.List (partition, findIndex, find, zipWith4)
import Data.Maybe (fromMaybe)
import qualified Data.Map as M
import Debug.Trace (trace)

seed =
  [".#."
  ,"..#"
  ,"###"
  ]

ruleLineMatch :: String -> [String] -> Maybe [String]
ruleLineMatch x rs = do
   i <- findIndex (\r -> r == x || (reverse r) == x) rs
   return $ (take i rs) ++ (drop (i+1) rs)

ruleMatch :: [String] -> [String] -> Bool
ruleMatch [] _ = True
ruleMatch (x:xs) rs = fromMaybe False $ do
  rs' <- ruleLineMatch x rs
  return $ ruleMatch xs rs'

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

part1 :: Rules -> [[String]]
part1 rules = let
  results = take 4 $ iterate (enhance rules) seed
  result = last $ results
  on = length $ filter (\x -> x == '#') $ concat result
  in results

printGrid :: [String] -> IO ()
printGrid g = do
  mapM_ putStrLn g
  putStrLn ""

run :: IO ()
run = do
  rulesRaw <- readFile "data/21.txt"
  let rules = parseInput . lines $ rulesRaw
  let results = part1 rules
  mapM_ printGrid results
  -- putStrLn $ "part1: " ++ (show (part1 rules))
