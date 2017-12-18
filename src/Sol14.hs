{-# LANGUAGE NamedFieldPuns #-}
module Sol14
    (run
    ) where

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

-- exampleKey :: String
-- exampleKey = "flqrgnkx"

-- example = let
--   g = grid (rowHashes exampleKey)
--   in calcUsed g

inputKey :: String
inputKey = "uugsqrei"

part1 :: Int
part1 = let
  g = grid (rowHashes inputKey)
  in calcUsed g

run :: IO ()
run = do
  putStrLn $ "part1: " ++ (show part1)
