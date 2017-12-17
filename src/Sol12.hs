{-# LANGUAGE NamedFieldPuns #-}
module Sol12
    (run
    ) where

import Data.List.Split (splitOn)
import qualified Data.Set as S

parseInput :: String -> [[Int]]
parseInput input = do
  l <- lines input
  return $ let
    [_, r] = splitOn " <-> " l
    in
    map read $ splitOn ", " r

visitAll :: [[Int]] -> S.Set Int -> [Int]-> S.Set Int
visitAll input visited [] = visited
visitAll input visited xs = let
  children = concatMap (\x -> input !! x) xs
  new = filter (\x -> S.notMember x visited) children
  in
  visitAll input (S.union visited (S.fromList new)) new

visitedFrom :: [[Int]] -> Int -> S.Set Int
visitedFrom input start = visitAll input (S.fromList [start]) [start]

part1 :: [[Int]] -> Int
part1 input = S.size $ visitedFrom input 0

-- Repeats the work but nice and clear
part2 :: [[Int]] -> Int
part2 input = S.size $ foldl (\s x -> S.union s (S.singleton $ visitedFrom input x)) S.empty [0..(length input - 1)]

run :: IO ()
run = do
  rawInput <- readFile "data/12.txt"
  let input = parseInput rawInput
  putStrLn $ "part1: " ++ (show (part1 input))
  putStrLn $ "part2: " ++ (show (part2 input))
