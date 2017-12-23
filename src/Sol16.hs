{-# LANGUAGE NamedFieldPuns #-}
module Sol16
    (run
    ) where

import qualified Data.Sequence as Sq
import qualified Data.Map.Strict as M
import Data.Foldable (toList)
import Data.Maybe (fromMaybe)
import Data.List.Split (splitOn)

data DanceStep = Spin Int | Exchange Int Int | Partner Char Char
data Line = Line
  { lSeq :: Sq.Seq Char
  , lIdx :: M.Map Char Int
  }

spin :: Line -> Int -> Line
spin (Line {lSeq, lIdx}) i = let
  len = length lSeq
  newList = take len $ drop (len - i) $ (cycle (toList lSeq))
  in makeLine newList

exchange :: Line -> Int -> Int -> Line
exchange (Line {lSeq, lIdx}) i j = let
  atI = Sq.index lSeq i
  atJ = Sq.index lSeq j
  in Line { lSeq = Sq.adjust (const atI) j . Sq.adjust (const atJ) i $ lSeq
          , lIdx = M.adjust (const j) atI . M.adjust (const i) atJ $ lIdx
          }

partner :: Line -> Char -> Char -> Line
partner line@(Line {lSeq, lIdx}) a b = fromMaybe line $ do
  idxA <- M.lookup a lIdx
  idxB <- M.lookup b lIdx
  return $ exchange line idxA idxB

danceStep :: Line -> DanceStep -> Line
danceStep line (Spin i) = spin line i
danceStep line (Exchange i j) = exchange line i j
danceStep line (Partner a b) = partner line a b

makeLine s =
  Line { lSeq = Sq.fromList s
       , lIdx = M.fromList $ zip s [0..]
       }

danceResult :: String -> [DanceStep] -> String
danceResult l steps =
  toList . lSeq $ foldl danceStep (makeLine l) steps

exampleSteps = [Partner 'a' 'b', Exchange 0 1]

example :: String
example = danceResult "abcde" exampleSteps

part1 :: [DanceStep] -> String
part1 steps = danceResult "abcdefghijklmnop" steps

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
  let dance = parseInput input
  putStrLn $ "part1: " ++ (show (part1 dance))
  putStrLn $ "part2: " ++ (show (part1 (concat (take 1000000000 (repeat dance)))))
