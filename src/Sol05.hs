module Sol05
    (run
    ) where

import Control.Monad.Primitive
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM

calcSteps :: PrimMonad m => (Int -> Int) -> Int -> Int -> V.MVector (PrimState m) Int -> m Int
calcSteps f c i xs =
  if i >= VM.length xs
  then return c
  else do
    j <- VM.read xs i
    VM.write xs i (f j)
    calcSteps f (c+1) (i+j) xs

parseInput f = V.thaw $ V.fromList $ do
  l <- lines f
  return $ (read l :: Int)

part1 :: PrimMonad m => V.MVector (PrimState m) Int -> m Int
part1 = calcSteps ((+) 1) 0 0

part2 :: PrimMonad m => V.MVector (PrimState m) Int -> m Int
part2 = calcSteps (\j -> if j >= 3 then j-1 else j+1) 0 0

run :: IO ()
run = do
  input <- readFile "data/05.txt"
  i1 <- parseInput input
  p1 <- part1 i1
  putStrLn (show p1)
  i2 <- parseInput input
  p2 <- part2 i2
  putStrLn (show p2)

