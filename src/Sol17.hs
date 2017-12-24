{-# LANGUAGE NamedFieldPuns #-}
module Sol17
    (run
    ) where

import Data.List (elemIndex, splitAt, foldl')
import Control.DeepSeq (deepseq)
import Control.Monad (foldM)
import qualified Data.Sequence as S

exampleInput = 3

input = 363

spinlock :: Int -> (Int, S.Seq Int) -> Int -> (Int, S.Seq Int)
spinlock spin (pos, xs) i =
  let
    l = length xs
    entry = (pos + spin) `rem` l + 1
    (before, after) = S.splitAt entry xs
    newlist = before S.>< S.singleton i S.>< after
  in (entry, newlist)

part1 :: Int -> Int -> Int
part1 spinSize times = let
  (p, vals) = foldl' (spinlock spinSize) (0, S.singleton 0) [1..times]
  in S.index vals (p+1)

spinlockLite :: Int -> (Int, Int) -> Int -> (Int, Int)
spinlockLite spin (pos, after0) i =
  let entry = (pos + spin) `rem` i + 1
  in (entry, if entry == 1 then i else after0)

part2 :: Int -> Int -> Int
part2 spinSize times = let
  (_, pos0) = foldl' (spinlockLite spinSize) (0, 0) [1..times]
  in pos0

example2 :: Int -> (Int, Int)
example2 i = foldl' (spinlockLite exampleInput) (0, 0) [1..i]

run :: IO ()
run = do
  putStrLn ("example: " ++ (show (part1 exampleInput 2017)))
  putStrLn ("part1: " ++ (show (part1 input 2017)))
  putStrLn ("part2: " ++ (show (part2 input 50000000)))
