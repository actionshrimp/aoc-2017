{-# LANGUAGE NamedFieldPuns #-}
module Sol13
    (run
    ) where

import Data.List (elemIndex)
import Data.List.Split (splitOn)
import Data.Maybe (catMaybes)
import qualified Data.Set as S

parseInput :: String -> [(Int, Int)]
parseInput input = do
  l <- lines input
  return $ let [a, b] = splitOn ": " l in (read a, read b)

exampleInput :: [(Int, Int)]
exampleInput = [(0, 3), (1, 2), (4, 4), (6, 4)]

severity :: Int -> (Int, Int) -> Maybe Int
severity delay (depth, range) =
  if (depth + delay) `rem` (2 * (range - 1)) == 0
  then Just (depth * range)
  else Nothing

severities :: [(Int, Int)] -> Int -> [Int]
severities input delay =
  catMaybes $ map (severity delay) input

part1 :: [(Int, Int)] -> Int
part1 input = foldl (+) 0 $ severities input 0

part2Brute :: [(Int, Int)] -> Maybe Int
part2Brute input = elemIndex [] $ map (severities input) [0..]

found :: [(Int, Int)] -> Int -> Bool
found [] delay = False
found (x@(depth, range):xs) delay =
  if (depth + delay) `rem` (2 * (range - 1)) == 0
  then True
  else found xs delay

part2Faster :: [(Int, Int)] -> Int
part2Faster input = head $ filter (not . found input) [0..]

run :: IO ()
run = do
  rawInput <- readFile "data/13.txt"
  let input = parseInput rawInput
  putStrLn $ "part1: " ++ (show (part1 input))
  putStrLn $ "part2: " ++ (show (part2Faster input))
