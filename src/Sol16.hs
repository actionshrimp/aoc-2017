{-# LANGUAGE NamedFieldPuns #-}
module Sol16
    (run
    ) where

import qualified Data.Sequence as Sq
import Data.Foldable (toList)
import Data.Maybe (fromMaybe)
import Data.List.Split (splitOn)

data DanceStep = Spin Int | Exchange Int Int | Partner Char Char
type Line = Sq.Seq Char

exchange :: Line -> Int -> Int -> Line
exchange line i j = let
  atI = Sq.index line i
  atJ = Sq.index line j
  in Sq.adjust (const atI) j . Sq.adjust (const atJ) i $ line

partner :: Line -> Char -> Char -> Line
partner line a b = fromMaybe line $ do
  idxA <- Sq.elemIndexL a line
  idxB <- Sq.elemIndexL b line
  return $ exchange line idxA idxB

danceStep :: Line -> DanceStep -> Line
danceStep line s =
  let l = length line in
    case s of
      Spin i -> Sq.fromList $ take l $ drop (l - i) $ (cycle (toList line))
      Exchange i j -> exchange line i j
      Partner a b -> partner line a b

exampleLine = Sq.fromList "abcde"
exampleInstrs = [Spin 1, Exchange 3 4, Partner 'e' 'b']

example :: Line
example = foldl danceStep exampleLine exampleInstrs

line = Sq.fromList "abcdefghijklmnop"

part1 :: [DanceStep] -> Line
part1 steps = foldl danceStep line steps

parseStep :: String -> DanceStep
parseStep ('s' : xs) = Spin (read xs)
parseStep ('x' : xs) = let [i, j] = splitOn "/" xs in Exchange (read i) (read j)
parseStep ('p' : xs) = let [a, b] = splitOn "/" xs in Partner (head a) (head b)

parseInput :: String -> [DanceStep]
parseInput s = do
  i <- splitOn "," s
  return $ parseStep i

run :: IO ()
run = do
  input <- readFile "data/16.txt"
  putStrLn $ "part1: " ++ (show (part1 $ parseInput input))
