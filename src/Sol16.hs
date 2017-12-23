{-# LANGUAGE NamedFieldPuns #-}
module Sol16
    (run
    ) where

import qualified Data.Sequence as Sq
import qualified Data.Map.Strict as M
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM
import qualified Data.Char as C
import Control.Monad (foldM, replicateM_, foldM_)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Foldable (toList)
import Data.Maybe (fromMaybe)
import Data.List.Split (splitOn)
import Debug.Trace (trace)

data DanceStep = Spin Int | Exchange Int Int | Partner Char Char
data Line = Line
  { lSeq :: VM.IOVector Char
  , lIdx :: VM.IOVector Int
  , offset :: IORef Int
  , l :: Int
  }

nameIdx :: Char -> Int
nameIdx c = C.ord c - 97

spin :: Line -> Int -> IO Line
spin line@(Line {offset, l}) i = do
  o <- readIORef offset
  writeIORef offset ((o + (l - i)) `rem` l)
  return line

exchange :: Line -> Int -> Int -> IO Line
exchange line@(Line {lSeq, lIdx, offset, l}) i j = do
  o <- readIORef offset
  let oi = ((i + o) `rem` l)
  let oj = ((j + o) `rem` l)
  atI <- VM.read lSeq oi
  atJ <- VM.read lSeq oj
  VM.write lSeq oi atJ
  VM.write lSeq oj atI
  VM.write lIdx (nameIdx atI) oj
  VM.write lIdx (nameIdx atJ) oi
  return line

partner :: Line -> Char -> Char -> IO Line
partner line@(Line {lSeq,lIdx}) a b = do
  let nia = (nameIdx a)
  let nib = (nameIdx b)
  idxA <- VM.read lIdx nia
  idxB <- VM.read lIdx nib
  VM.write lIdx nib idxA
  VM.write lIdx nia idxB
  VM.write lSeq idxA b
  VM.write lSeq idxB a
  return line

danceStep :: Line -> DanceStep -> IO Line
danceStep line (Spin i) = spin line i
danceStep line (Exchange i j) = exchange line i j
danceStep line (Partner a b) = partner line a b

makeLine :: String -> IO Line
makeLine s = do
  x <- (V.thaw (V.fromList s))
  y <- (V.thaw (V.fromList (take (length s) [0..])))
  o <- newIORef 0
  return $ Line { lSeq = x
                , lIdx = y
                , offset = o
                , l = length s
                }

readLine :: Line -> IO String
readLine line@(Line {lSeq, offset, l}) = do
  o <- readIORef offset
  s <- V.freeze lSeq
  return $ V.toList (V.slice o (l-o) s) ++ V.toList (V.slice 0 o s)

danceResult :: String -> [DanceStep] -> IO String
danceResult s steps = do
  line <- makeLine s
  r <- foldM danceStep line steps
  readLine r

-- s1, a spin of size 1: eabcd.
-- x3/4, swapping the last two programs: eabdc.
-- pe/b, swapping programs e and b: baedc.
exampleSteps =
  [ Spin 1
  , Exchange 3 4
  , Partner 'e' 'b'
  ]

example :: IO String
example = danceResult "abcde" exampleSteps

part1 :: [DanceStep] -> IO String
part1 steps = danceResult "abcdefghijklmnop" steps

parseStep :: String -> DanceStep
parseStep ('s' : xs) = Spin (read xs)
parseStep ('x' : xs) = let [i, j] = splitOn "/" xs in Exchange (read i) (read j)
parseStep ('p' : xs) = let [a, b] = splitOn "/" xs in Partner (head a) (head b)

parseInput :: String -> [DanceStep]
parseInput s = do
  i <- splitOn "," s
  return $ parseStep i

part2 :: Int -> [DanceStep] -> IO String
part2 i steps = do
  let s = "abcdefghijklmnop"
  line <- makeLine s
  let go j = do
        foldM_ danceStep line steps
        newLine <- readLine line
        if newLine == s then return j else go (j+1)
  loopN <- go 1
  putStrLn $ "found loop at: " ++ (show loopN)
  let remainder = i `rem` loopN
  replicateM_ remainder (foldM_ danceStep line steps)
  readLine line

run :: IO ()
run = do
  input <- readFile "data/16.txt"
  let dance = parseInput input
  p1 <- part1 dance
  putStrLn $ "part1: " ++ (show p1)
  p2 <- part2 1000000000 dance
  putStrLn $ "part2: " ++ (show p2)
