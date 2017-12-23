{-# LANGUAGE NamedFieldPuns, BinaryLiterals #-}
module Sol15
    (run
    ) where

import Data.List.Split (splitOn)
import qualified Data.Bits as B
import qualified Data.Word as W
import qualified Data.Set as S


seeds = [ 679, 771 ]
factors = [ 16807, 48271 ]
divisor = 2147483647

mask :: W.Word32
mask = 0b00000000000000001111111111111111

gen :: Int -> Int -> Int -> [W.Word32]
gen seed f d =
  let newSeed = ((seed * f) `rem` d) in
  (fromIntegral newSeed) : (gen newSeed f d)

rightmostMatch :: W.Word32 -> W.Word32 -> Bool
rightmostMatch a b =
  (a `B.xor` (B.complement b)) B..&. mask == mask

part1Example :: [Bool]
part1Example = let
  a = (gen 65   (factors !! 0) divisor)
  b = (gen 8921 (factors !! 1) divisor)
  in take 5 $ zipWith rightmostMatch a b

part1 :: Int
part1 = let
  a = (gen (seeds !! 0) (factors !! 0) divisor)
  b = (gen (seeds !! 1) (factors !! 1) divisor)
  in length $ filter id $ take 40000000 $ zipWith rightmostMatch a b

run :: IO ()
run = do
  putStrLn $ "part1: " ++ (show part1)
