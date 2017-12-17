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

workPart1 :: [[Int]] -> S.Set Int -> [Int]-> S.Set Int
workPart1 input visited [] = visited
workPart1 input visited xs = let
  children = concatMap (\x -> input !! x) xs
  new = filter (\x -> S.notMember x visited) children
  in
  workPart1 input (S.union visited (S.fromList new)) new

part1 :: [[Int]] -> Int
part1 input = let
  visited = workPart1 input (S.fromList [0]) [0]
  in
  S.size visited

run :: IO ()
run = do
  rawInput <- readFile "data/12.txt"
  let input = parseInput rawInput
  putStrLn $ "part1: " ++ (show (part1 input))
