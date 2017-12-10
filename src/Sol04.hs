module Sol04
    (run
    ) where

import Control.Monad (guard)
import Data.Set as S
import Data.List (permutations)


validPhrasePart1 :: [String] -> Bool
validPhrasePart1 p = (S.size (S.fromList p) == length p)

validPhrasePart2 :: [String] -> Bool
validPhrasePart2 (p : ps) = checkPhrase p ps
  where
    checkPhrase p [] = True
    checkPhrase p ps@(q:qs) =
      all (not . elem p . permutations) ps && checkPhrase q qs

validPhrases :: ([String] -> Bool) -> [[String]] -> [[String]]
validPhrases validPhrase phrases = do
  p <- phrases
  guard (validPhrase $ p)
  return p

parsePhrases :: String -> [[String]]
parsePhrases pfile = do
  l <- lines pfile
  return $ do
    words l

part1 :: [[String]] -> Int
part1 = length . validPhrases validPhrasePart1

part2 :: [[String]] -> Int
part2 = length . validPhrases validPhrasePart2

run :: IO ()
run = do
  input <- readFile "data/04.txt"
  putStrLn (show (part2 . parsePhrases $ input))
