{-# LANGUAGE NamedFieldPuns #-}
module Sol19
    (run
    ) where

import Data.List (elemIndex)
import qualified Data.Sequence as Sq

data Dir = D | L | U | R | Done
  deriving (Show, Eq)

idx :: [a] -> Int -> Maybe a
idx (x:_) 0 = Just x
idx [] _ = Nothing
idx (x:xs) n = idx xs (n-1)

start = (D, (0, 27))

letters = ['A'..'Z']

rlkp :: [[Char]] -> (Int, Int) -> Maybe Char
rlkp route (y, x) = do
  row <- route `idx` y
  cell <- row `idx` x
  return $ cell

cdir :: [[Char]] -> (Dir, (Int, Int)) -> (Dir, (Int, Int))
cdir route (d, (y, x)) | d == D || d == U = case (rlkp route (y, x+1), rlkp route (y, x-1)) of
  (Just '-', _) -> (R, (y, x+1))
  (_, Just '-') -> (L, (y, x-1))
cdir route (d, (y, x)) | d == L || d == R = case (rlkp route (y+1, x), rlkp route (y-1, x)) of
  (Just '|', _) -> (D, (y+1, x))
  (_, Just '|') -> (U, (y-1, x))

samedir :: (Dir, (Int, Int)) -> (Dir, (Int, Int))
samedir (D, (y, x)) = (D, (y+1, x))
samedir (U, (y, x)) = (U, (y-1, x))
samedir (L, (y, x)) = (L, (y, x-1))
samedir (R, (y, x)) = (R, (y, x+1))

walk :: [[Char]] -> (Dir, (Int, Int)) -> [Char]
walk route (d, z@(y, x)) = let
  Just item = rlkp route z
  in case item of
       '+' -> walk route (cdir route (d, z))
       i | i `elem` letters -> i : walk route (samedir (d, z))
       ' ' -> []
       _ -> walk route (samedir (d, z))

run :: IO ()
run = do
  input <- readFile "data/19.txt"
  let route = lines input
  putStrLn ("part1: " ++ (show (walk route start)))
