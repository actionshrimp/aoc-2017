module Sol05
    (sol05
    ) where

import Data.Sequence as Seq

calcSteps :: (Int -> Int) -> Int -> Int -> Seq Int -> Int
calcSteps f c i xs = if i >= Seq.length xs
  then c
  else let j = Seq.index xs i
       in calcSteps f (c+1) (i+j) (Seq.update i (f j) xs)

parseInput f = Seq.fromList $ do
  l <- lines f
  return $ (read l :: Int)

part1 :: Seq Int -> Int
part1 = calcSteps ((+) 1) 0 0

part2 :: Seq Int -> Int
part2 = calcSteps (\j -> if j >= 3 then j-1 else j+1) 0 0

sol05 :: IO ()
sol05 = do
  input <- readFile "data/05.txt"
  putStrLn (show . part1 . parseInput $ input)
  putStrLn (show . part2 . parseInput $ input)

