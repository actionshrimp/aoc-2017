{-# LANGUAGE NamedFieldPuns, ScopedTypeVariables, LambdaCase, MultiWayIf #-}
module Sol22
    (run
    ) where

import qualified Data.Map.Strict as M
import Data.Maybe (catMaybes, fromMaybe)

data Carrier = Carrier { dir :: (Int, Int), pos :: (Int, Int) }
  deriving Show

type Infected = M.Map (Int, Int) Bool

seed :: String -> (Int,Carrier, Infected)
seed s = let
  grid = lines s
  mapped = zipWith (\y row -> zipWith (\x c -> if c == '#' then Just ((x, y), True) else Nothing) [0..] row) [0..] $ grid
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

step :: (Int, Carrier, Infected) -> (Int, Carrier, Infected)
step (x, c@Carrier {dir, pos = pos@(px, py)}, inf) = let
  infected = M.findWithDefault False pos inf
  dir'@(d'x, d'y) = if infected then turnRight dir else turnLeft dir
  inf' = if infected then M.delete pos inf else M.insert pos True inf
  x' = if infected then x else x + 1
  in (x', c {dir = dir', pos = (px + d'x, py + d'y)}, inf')

exampleSeed = seed $ "..#\n#..\n..."

causedInfection seed steps = let
  (x, c, inf) = head . drop steps $ iterate step seed
  in x

example = causedInfection exampleSeed 10000

run :: IO ()
run = do
  seedRaw <- readFile "data/22.txt"
  putStrLn $ "part1: " ++ (show (causedInfection (seed seedRaw) 10000))
