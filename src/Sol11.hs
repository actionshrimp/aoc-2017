{-# LANGUAGE NamedFieldPuns #-}
module Sol11
    (run
    ) where

import Data.List.Split (splitOn)
import Data.Maybe (fromMaybe, catMaybes)
import qualified Data.Map as M
import Debug.Trace (trace)

addStepType :: (Int, Int) -> String -> (Int, Int)
addStepType (n, w)  "n" = (n + 2, w    )
addStepType (n, w) "nw" = (n + 1, w + 1)
addStepType (n, w) "ne" = (n + 1, w - 1)
addStepType (n, w)  "s" = (n - 2, w    )
addStepType (n, w) "se" = (n - 1, w - 1)
addStepType (n, w) "sw" = (n - 1, w + 1)

distance :: (Int, Int) -> Int
distance (n, w) = let total = abs n + abs w in
  total `div` 2 + total `rem` 2

part1 :: [String] -> Int
part1 input = let
  coords = foldl addStepType (0, 0) input
  in distance coords

addStepWithMax :: (Int, (Int, Int)) -> String -> (Int, (Int, Int))
addStepWithMax (maxDist, coords) x = let
  newCoords = addStepType coords x
  newDist = distance newCoords
  in
    if newDist > maxDist
    then (newDist, newCoords)
    else (maxDist, newCoords)

part2 :: [String] -> Int
part2 input = let
  (max, _) = foldl addStepWithMax (0, (0, 0)) input
  in max

example1 = part1 ["ne", "ne", "ne"]             -- 3
example2 = part1 ["ne", "ne", "sw", "sw"]       -- 0
example3 = part1 ["ne", "ne", "s", "s"]         -- 2
example4 = part1 ["se", "sw", "se", "sw", "sw"] -- 3

allExamples = [example1, example2, example3, example4]

parseInput :: String -> [String]
parseInput s = splitOn "," $ head $ lines s

run :: IO ()
run = do
  rawInput <- readFile "data/11.txt"
  let input = parseInput rawInput
  putStrLn $ "part1: " ++ (show (part1 input))
  putStrLn $ "part2: " ++ (show (part2 input))
