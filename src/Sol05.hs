module Sol05
    (sol05
    ) where

import Data.Sequence as Seq

runPart1 :: Int -> Int -> Seq Int -> Int
runPart1 c i xs = if i >= Seq.length xs
  then c
  else let j = Seq.index xs i
       in runPart1 (c+1) (i+j) (Seq.update i (j+1) xs)

parseInput f = Seq.fromList $ do
  l <- lines f
  return $ (read l :: Int)

part1 :: Seq Int -> Int
part1 = runPart1 0 0

sol05 :: IO ()
sol05 = do
  input <- readFile "data/05.txt"
  putStrLn (show . part1 . parseInput $ input)
