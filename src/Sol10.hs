{-# LANGUAGE NamedFieldPuns #-}
module Sol10
    (run
    ) where

import Data.List.Split (splitOn, chunksOf)
import Data.Bits (xor)
import Numeric (showHex)
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

runHashFn :: Int -> [Int] -> HashSt
runHashFn ropeLen ls = (foldl hashFn (initHashSt ropeLen) ls)

rawInput = "130,126,1,11,140,2,255,207,18,254,246,164,29,104,0,224"

part1Input :: [Int]
part1Input = map read $ splitOn "," rawInput

asciiCode :: Char -> Int
asciiCode = fromEnum

extraLengths :: [Int]
extraLengths = [17, 31, 73, 47, 23]

part2Input :: [Int]
part2Input = (map asciiCode rawInput) ++ extraLengths

example :: Int
example = let (a:b:_) = (rope (runHashFn 5 [3, 4, 1, 5])) in
  a * b

part1 :: Int
part1 = let (a:b:_) = (rope (runHashFn 256 part1Input)) in
  a * b

int256AsHexStr :: Int -> String
int256AsHexStr x | x < 16 = "0" ++ showHex x ""
                 | otherwise = showHex x ""

knotHash :: String -> String
knotHash input = let
  fullInput = (map asciiCode input ++ extraLengths)
  sparseHash = (rope (runHashFn 256 (take (64 * (length fullInput)) (cycle fullInput))))
  denseHash = map (foldl1 xor) (chunksOf 16 sparseHash)
  in concatMap int256AsHexStr denseHash

part2 :: String
part2 = knotHash rawInput

run :: IO ()
run = do
  putStrLn $ "part 1: " ++ show part1
  putStrLn $ "part 2: " ++ show part2
