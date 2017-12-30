{-# LANGUAGE NamedFieldPuns, ScopedTypeVariables, LambdaCase, MultiWayIf #-}
module Sol24
    (run
    ) where

import Data.List.Split (splitOn)
import Data.List (delete, nub, sort)
import Data.Foldable (foldl')

--Components
type Cs = [(Int, Int)]

parseInput :: String -> Cs
parseInput s = [(read a, read b) | l  <- lines s, let [a, b] = splitOn "/" l ]

expandOne :: (Cs, Cs) -> Maybe [(Cs, Cs)]
expandOne (available, current@((_, cp):cs)) =
  case filter (\(apa, apb) -> apa == cp || apb == cp) available of
    [] -> Nothing
    matches -> Just $ map (\a@(apa, apb) -> (delete a available, if apa == cp then a:current else (apb, apa):current)) matches

expandMany :: [(Cs, Cs)] -> [(Cs, Cs)]
expandMany work = foldl' f [] work where
  f moreWork w = case expandOne w of
    Nothing -> moreWork
    Just exps -> exps ++ moreWork

validBridges' :: Cs -> [Cs]
validBridges' cs = delete [] $ go [(cs, [(0, 0)])] where
  go [] = []
  go work = map (init . snd) work ++ go (expandMany work)

exampleComponentsRaw = "\
\0/2\n\
\2/2\n\
\2/3\n\
\3/4\n\
\3/5\n\
\0/1\n\
\10/1\n\
\9/10"

strength :: Cs -> Int
strength cs = sum $ map (\(a, b) -> a+b) cs

strongest :: [Cs] -> Int
strongest bs = head . reverse . sort $ map strength bs

run :: IO ()
run = do
  inputRaw <- readFile "data/24.txt"
  putStrLn "example: "
  let exampleBridges = (validBridges' (parseInput exampleComponentsRaw))
  -- mapM_ (putStrLn . show) exampleBridges
  putStrLn $ "max strength: " ++ (show (strongest exampleBridges))
  putStrLn ""
  putStrLn "part1: "
  let p1Bridges = (validBridges' (parseInput inputRaw))
  -- mapM_ (putStrLn . show) p1Bridges
  putStrLn $ "max strength: " ++ (show (strongest p1Bridges))
