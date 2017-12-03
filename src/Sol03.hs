module Sol03
    (sol03
    ) where

input :: Int
input = 277678

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

--Data from square 1 is carried 0 steps, since it's at the access port.
--Data from square 12 is carried 3 steps, such as: down, left, left.
--Data from square 23 is carried only 2 steps: up twice.
--Data from square 1024 must be carried 31 steps.

part1 = distance input

sol03 :: IO ()
sol03 = putStrLn (show part1)
