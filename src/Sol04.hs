module Sol04
    (sol04
    ) where

import Control.Monad (guard)
import Data.Set as S


validPhrasePart1 :: [String] -> Bool
validPhrasePart1 p = (S.size (S.fromList p) == length p)

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

sol04 :: IO ()
sol04 = do
  part1Input <- readFile "data/04-part1.txt"
  putStrLn (show (part1 . parsePhrases $ part1Input))
