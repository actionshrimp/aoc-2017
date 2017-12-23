{-# LANGUAGE NamedFieldPuns #-}
module Sol14
    (run
    ) where

import Data.Maybe (fromMaybe)
import Debug.Trace (trace)
import qualified Data.Set as S
import qualified Data.Map as M
import Sol10 (knotHash)

rowHashes :: String -> [String]
rowHashes key = map (knotHash . (\x -> key ++ "-" ++ (show x))) [0..127]

hashToBin '0' = [0, 0, 0, 0]
hashToBin '1' = [0, 0, 0, 1]
hashToBin '2' = [0, 0, 1, 0]
hashToBin '3' = [0, 0, 1, 1]
hashToBin '4' = [0, 1, 0, 0]
hashToBin '5' = [0, 1, 0, 1]
hashToBin '6' = [0, 1, 1, 0]
hashToBin '7' = [0, 1, 1, 1]
hashToBin '8' = [1, 0, 0, 0]
hashToBin '9' = [1, 0, 0, 1]
hashToBin 'a' = [1, 0, 1, 0]
hashToBin 'b' = [1, 0, 1, 1]
hashToBin 'c' = [1, 1, 0, 0]
hashToBin 'd' = [1, 1, 0, 1]
hashToBin 'e' = [1, 1, 1, 0]
hashToBin 'f' = [1, 1, 1, 1]

grid :: [String] -> [[Int]]
grid hashes = map (concatMap hashToBin) hashes

calcUsed :: [[Int]] -> Int
calcUsed g = foldl1 (+) $ map (foldl1 (+)) g

inputKey :: String
inputKey = "uugsqrei"

part1 :: Int
part1 = let
  g = grid (rowHashes inputKey)
  in calcUsed g

neighbours :: (Int, Int) -> [(Int, Int)]
neighbours (x, y) = [(x, y-1), (x, y+1), (x-1, y), (x+1, y)]

gridLookup :: [[Int]] -> M.Map (Int, Int) Bool
gridLookup as = let
  indexed = zipWith (\y b -> zipWith (\x a -> ((x, y), a == 1)) [0..] b) [0..] as
  in
  foldl1 (\a b -> M.unionWith (||) b a) $ map M.fromList indexed

filled :: M.Map (Int, Int) Bool -> (Int, Int) -> Bool
filled lookup x = fromMaybe False (M.lookup x lookup)

joinedWith :: M.Map (Int, Int) Bool -> S.Set (Int, Int) -> S.Set (Int, Int)
joinedWith lookup us = let
  neighboursIfFilled u =
    if (filled lookup u) then neighbours u else []
  ns = S.fromList $ concatMap neighboursIfFilled us
  in S.union us ns

toFixedPoint :: Eq a => (a -> a) -> a -> a
toFixedPoint f x = let y = f x in
  if y == x then x else toFixedPoint f y

calcRegions :: [[Int]] -> Int
calcRegions g = let
  l = gridLookup (grid (rowHashes inputKey))
  unvisited = S.fromList [(x, y)
                         | x <- [0..(length (head g) - 1)]
                         , y <- [0..(length g - 1)]
                         , (filled l (x, y)) ]
  go l n us
    | S.null us = n
    | otherwise = let
        u = S.singleton $ head $ S.toList us
        vs = toFixedPoint (joinedWith l) u in
            go l (n+1) (S.difference us vs)
  in go l 0 unvisited

part2 :: Int
part2 = calcRegions (grid (rowHashes inputKey))

run :: IO ()
run = do
  putStrLn $ "part1: " ++ (show part1)
  putStrLn $ "part2: " ++ (show part2)
