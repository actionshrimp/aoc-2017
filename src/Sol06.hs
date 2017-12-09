module Sol06
    (sol06
    ) where

import Data.Function (on)
-- import Data.Ord (compare)
import Data.List (maximumBy)
import Data.Tuple (fst)
import qualified Data.Vector as V
import qualified Data.Map as M

input :: V.Vector Int
input = V.fromList [10, 3, 15, 10, 5, 15, 5, 15, 9, 2, 5, 8, 5, 2, 3, 6]

exampleInput :: V.Vector Int
exampleInput = V.fromList [0, 2, 7, 0]

firstLargest = (compare `on` fst) `mappend` (flip (compare `on` snd))

redistribute :: V.Vector Int -> V.Vector Int
redistribute xs = let
  (mVal, mIdx) = maximumBy firstLargest $ zip (V.toList xs) [0 ..]
  nextIdxs = (map (\x -> x `rem` V.length xs) [mIdx ..])
  ops = zip  nextIdxs ([negate mVal] ++ (take mVal (repeat 1)))
  in V.accum (+) xs ops

firstAlreadySeen :: [V.Vector Int] -> (Int, V.Vector Int)
firstAlreadySeen xs = go 0 M.empty xs
  where go n d (x:xs) =
          if M.member x d
          then (n, x)
          else go (n+1) (M.insert x True d) xs

part1 = fst $ firstAlreadySeen (iterate redistribute input)

part2 = let (_, loopStart) = firstAlreadySeen (iterate redistribute input)
  in fst $ firstAlreadySeen (iterate redistribute loopStart)

sol06 :: IO ()
sol06 = do
  putStrLn (show part1)
  putStrLn (show part2)

