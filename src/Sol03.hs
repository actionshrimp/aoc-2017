module Sol03
    (run
    ) where

import qualified Data.Map as M
import Data.Maybe (catMaybes, fromMaybe)

input :: Int
input = 277678

--Data from square 1 is carried 0 steps, since it's at the access port.
--Data from square 12 is carried 3 steps, such as: down, left, left.
--Data from square 23 is carried only 2 steps: up twice.
--Data from square 1024 must be carried 31 steps.

distance :: Int -> Int
distance n =
  let
    squareSize = head . filter (\x -> x^2 >= n) $ [1, 3 ..]
    walkMax = squareSize - 1
    walkMin = squareSize `div` 2
    edgePosBack = squareSize ^ 2 - n
    edgeDistances = cycle ([walkMax, walkMax - 1 .. walkMin] ++
                           [walkMin + 1, walkMin + 2 .. walkMax - 1])
  in
    edgeDistances !! edgePosBack

part1 = distance input

-- 147  142  133  122   59
-- 304    5    4    2   57
-- 330   10    1    1   54
-- 351   11   23   25   26
-- 362  747  806--->   ...

data Dir = D | L | U | R

neighbours (i, j) = [
  (i+1, j+1), (i, j+1), (i-1, j+1),
  (i+1, j  ),           (i-1, j  ),
  (i+1, j-1), (i, j-1), (i-1, j-1)]

updateMap :: (Int, Int) -> M.Map (Int, Int) Int -> M.Map (Int, Int) Int
updateMap c@(i, j) m =
  if (i, j) == (0, 0) then
    M.insert c 1 m
  else let
    neighbouringVals = map (\c -> M.lookup c m) (neighbours c)
    total = foldl (+) 0 (catMaybes neighbouringVals)
  in
    M.insert c total m

go target m c d = let
  m' = updateMap c m
  t = fromMaybe 0 (M.lookup c m')
  in [(c, t)] ++ (if t >= target then [] else go' target m' c d)

go' t m c@(i, j) R = go t m ((i+1), j) (if i == j then U else R)
go' t m c@(i, j) U = go t m (i, (j-1)) (if i == ((negate j)+1) then L else U)
go' t m c@(i, j) L = go t m ((i-1), j) (if i == j+1 then D else L)
go' t m c@(i, j) D = go t m (i, (j+1)) (if (negate i) == j+1 then R else D)

theSeq = go 277678 M.empty (0, 0) R

part2 = last theSeq

run :: IO ()
run = putStrLn (show part2)
