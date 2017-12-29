{-# LANGUAGE NamedFieldPuns, ScopedTypeVariables, LambdaCase, MultiWayIf #-}
module Sol22
    (run
    ) where

import qualified Data.Map.Strict as M
import Data.Maybe (catMaybes, fromMaybe)

data Carrier = Carrier { dir :: (Int, Int), pos :: (Int, Int) }
  deriving Show

data State = Weakened | Infected | Flagged

type States = M.Map (Int, Int) State

seed :: String -> (Int, Carrier, States)
seed s = let
  grid = lines s
  mapped = zipWith (\y row -> zipWith (\x c -> if c == '#' then Just ((x, y), Infected) else Nothing) [0..] row) [0..] $ grid
  (x, y) = ((length (head grid)) `div` 2, length grid `div` 2)
  in (0, Carrier { pos = (x, y), dir = (0, -1)}, M.fromList . catMaybes . concat $ mapped)

--  -->x
--  |
--  v
--  y
turnRight :: (Int, Int) -> (Int, Int)
turnRight ( 0,  1) = (-1,  0)
turnRight ( 0, -1) = ( 1,  0)
turnRight ( 1,  0) = ( 0,  1)
turnRight (-1,  0) = ( 0, -1)

turnLeft :: (Int, Int) -> (Int, Int)
turnLeft ( 0,  1) = ( 1,  0)
turnLeft ( 0, -1) = (-1,  0)
turnLeft ( 1,  0) = ( 0, -1)
turnLeft (-1,  0) = ( 0,  1)

stepPart1 :: (Int, Carrier, States) -> (Int, Carrier, States)
stepPart1 (x, c@Carrier {dir, pos = pos@(px, py)}, inf) = let
  state = M.lookup pos inf
  dir'@(d'x, d'y) = case state of
    Just Infected -> turnRight dir
    Nothing -> turnLeft dir
  inf' = case state of
    Just Infected -> M.delete pos inf
    Nothing -> M.insert pos Infected inf
  x' = case state of
    Just Infected -> x
    Nothing -> x + 1
  in (x', c {dir = dir', pos = (px + d'x, py + d'y)}, inf')

stepPart2 :: (Int, Carrier, States) -> (Int, Carrier, States)
stepPart2 (x, c@Carrier {dir, pos = pos@(px, py)}, inf) = let
  state = M.lookup pos inf
  dir'@(d'x, d'y) = case state of
    Nothing -> turnLeft dir
    Just Weakened -> dir
    Just Infected -> turnRight dir
    Just Flagged -> turnRight . turnRight $ dir
  inf' = case state of
    Nothing -> M.insert pos Weakened inf
    Just Weakened -> M.update (const (Just Infected)) pos inf
    Just Infected -> M.update (const (Just Flagged)) pos inf
    Just Flagged -> M.delete pos inf
  x' = case state of
    Just Weakened -> x + 1
    _ -> x
  in (x', c {dir = dir', pos = (px + d'x, py + d'y)}, inf')

exampleSeed = seed $ "..#\n#..\n..."

causedInfection stepper seed steps = let
  (x, c, inf) = head . drop steps $ iterate stepper seed
  in x

examplePart1 = causedInfection stepPart1 exampleSeed 10000
examplePart2 = causedInfection stepPart2 exampleSeed 10000000

run :: IO ()
run = do
  seedRaw <- readFile "data/22.txt"
  putStrLn $ "examplePart1: " ++ (show examplePart1)
  putStrLn $ "part1: " ++ (show (causedInfection stepPart1 (seed seedRaw) 10000))
  putStrLn $ "examplePart2: " ++ (show examplePart2)
  putStrLn $ "part2: " ++ (show (causedInfection stepPart2 (seed seedRaw) 10000000))
