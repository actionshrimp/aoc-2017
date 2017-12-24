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

part2 :: Int -> Int -> Maybe Int
part2 spinSize times = let
  (p, vals) = foldl' (spinlock spinSize) (0, S.singleton 0) [1..times]
  in do
    pos0 <- S.elemIndexL 0 vals
    return $ S.index vals (pos0 + 1)

run :: IO ()
run = do
  putStrLn ("example: " ++ (show (part1 exampleInput 2017)))
  putStrLn ("part1: " ++ (show (part1 input 2017)))
  putStrLn ("part2: " ++ (show (part2 input 50000000)))
