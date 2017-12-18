{-# LANGUAGE NamedFieldPuns #-}
module Sol13
    (run
    ) where

import Data.List.Split (splitOn)
import qualified Data.Set as S

parseInput :: String -> [(Int, Int)]
parseInput input = do
  l <- lines input
  return $ let [a, b] = splitOn ": " l in (read a, read b)

data Layer = Scanner { y :: Int, range :: Int, direction :: Int } | NoScanner
  deriving Show

initState :: [(Int, Int)] -> [Layer]
initState input = go 0 input where
    go i [] = []
    go i xs@((j, r):ys) =
      (if i == j
      then [Scanner { range = r, direction = 1, y = 0 }] ++ go (i+1) ys
      else [NoScanner] ++ go (i+1) xs)

stepLayer :: Layer -> Layer
stepLayer NoScanner = NoScanner
stepLayer s@Scanner{y, range, direction} =
  if y + direction < 0 || y + direction > (range - 1)
  then s { y = y - direction, direction = -direction}
  else s { y = y + direction }

stepFirewall :: [Layer] -> [Layer]
stepFirewall fw = map stepLayer fw

walkWall :: (Int, Int) -> [Layer] -> [Int]
walkWall _ [] = []
walkWall (wx,wy) (l:ls) =
  (case l of
    NoScanner -> []
    s@Scanner{y, range} -> if wy == y then [wx * range] else [])
  ++ walkWall (wx+1, wy) (stepFirewall ls)

exampleInput = [(0, 3), (1, 2), (4, 4), (6, 4)]

part1 :: [(Int, Int)] -> Int
part1 input = let
  is = initState input
  in foldl (+) 0 $ walkWall (0, 0) is

run :: IO ()
run = do
  rawInput <- readFile "data/13.txt"
  let input = parseInput rawInput
  putStrLn $ "part1: " ++ (show (part1 input))
  -- putStrLn $ "part2: " ++ (show (part2 input))
