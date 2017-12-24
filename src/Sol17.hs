{-# LANGUAGE NamedFieldPuns #-}
module Sol17
    (run
    ) where

exampleInput = 3

input = 363

spinlock :: Int -> (Int, [Int]) -> Int -> (Int, [Int])
spinlock spin (pos, xs) i =
  let
    l = length xs
    entry = (pos + spin) `rem` l + 1
  in
    (entry, take entry xs ++ [i] ++ drop entry xs)

part1 :: Int -> Int -> Int
part1 spinSize times = let
  (p, vals) = foldl (spinlock spinSize) (0, [0]) [1..times]
  in vals !! (p+1)

run :: IO ()
run = do
  putStrLn ("example: " ++ (show (part1 exampleInput 2017)))
  putStrLn ("part1: " ++ (show (part1 input 2017)))
