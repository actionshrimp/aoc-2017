{-# LANGUAGE NamedFieldPuns #-}
module Sol10
    (run
    ) where

import Data.List.Split (splitOn)
import Debug.Trace (trace)

data HashSt
  = HashSt { rope :: [Int]
           , pos :: Int
           , skip :: Int }
  deriving Show

initHashSt ropeLen = HashSt { rope = [0..(ropeLen - 1)], pos = 0, skip = 0}

hashFn :: HashSt -> Int -> HashSt
hashFn x@(HashSt { rope, pos, skip }) l = let
  ropeLen = length rope
  cr = cycle rope
  offsetNewRope = reverse (take l (drop pos cr)) ++ take (ropeLen - l) (drop (pos + l) cr) in
  x { rope = take ropeLen $ drop (ropeLen - pos) $ cycle offsetNewRope
    , pos = (pos + (l + skip)) `rem` ropeLen
    , skip = skip + 1}

parseInput :: String -> [Int]
parseInput i = map read $ splitOn "," i

runHashFn :: Int -> [Int] -> HashSt
runHashFn ropeLen ls = (foldl hashFn (initHashSt ropeLen) ls)

rawInput = "130,126,1,11,140,2,255,207,18,254,246,164,29,104,0,224"

example :: Int
example = let (a:b:_) = (rope (runHashFn 5 [3, 4, 1, 5])) in
  a * b

part1 :: [Int] -> Int
part1 input = let (a:b:_) = (rope (runHashFn 256 input)) in
  a * b

run :: IO ()
run = let
  input = (parseInput rawInput)
  in do
  putStrLn $ "part 1: " ++ show (part1 input)
