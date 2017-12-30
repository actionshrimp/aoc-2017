{-# LANGUAGE NamedFieldPuns, ScopedTypeVariables, LambdaCase, MultiWayIf #-}
module Sol24
    (run
    ) where

import Data.List.Split (splitOn)
import Data.List (delete, nub, sort, sortBy)
import Data.Foldable (foldl')
import Data.Ord (comparing)
import Data.Maybe (fromMaybe, fromJust)
import qualified Data.Sequence as Seq

seqInit :: Seq.Seq a -> Seq.Seq a
seqInit xs = let l = Seq.length xs in Seq.deleteAt (l-1) xs

seqMap :: (a -> b) -> Seq.Seq a -> Seq.Seq b
seqMap f xs = Seq.mapWithIndex (\i x -> f x) xs

seqHead :: Seq.Seq a -> a
seqHead = fromJust . (Seq.lookup 0)

seqFoldl :: (b -> a -> b) -> b -> Seq.Seq a -> b
seqFoldl f = Seq.foldlWithIndex (\acc i x -> f acc x)

seqDelete :: Eq a => a -> Seq.Seq a -> Seq.Seq a
seqDelete x xs = fromMaybe xs $ do
  i <- Seq.elemIndexL x xs
  return $ Seq.deleteAt i xs

--Components
type C = (Int, Int)
type Cs = Seq.Seq C

parseInput :: String -> Cs
parseInput s = Seq.fromList [(read a, read b) | l  <- lines s, let [a, b] = splitOn "/" l ]

expandOne :: (Cs, Cs) -> Maybe (Seq.Seq (Cs, Cs))
expandOne (available, current) = let
  (_, cp) = seqHead current
  matches = Seq.filter (\(apa, apb) -> apa == cp || apb == cp) available in
  if | matches == Seq.empty -> Nothing
     | otherwise -> Just $ seqMap (\a@(apa, apb) -> (seqDelete a available, if apa == cp then a Seq.<| current else (apb, apa) Seq.<| current)) matches

expandMany :: Seq.Seq (Cs, Cs) -> Seq.Seq (Cs, Cs)
expandMany work = seqFoldl f Seq.empty work where
  f moreWork w = case expandOne w of
    Nothing -> moreWork
    Just exps -> exps Seq.>< moreWork

validBridges' :: Cs -> Seq.Seq (Cs)
validBridges' cs = Seq.drop 1 $ go (Seq.singleton (cs, Seq.singleton (0, 0))) where
  go work | work == Seq.empty = Seq.empty
          | otherwise = seqMap (seqInit . snd) work Seq.>< go (expandMany work)

exampleComponentsRaw = "\
\0/2\n\
\2/2\n\
\2/3\n\
\3/4\n\
\3/5\n\
\0/1\n\
\10/1\n\
\9/10"

ecs = parseInput exampleComponentsRaw

strength :: Cs -> Int
strength cs = sum $ seqMap (\(a, b) -> a+b) cs

strongest :: Seq.Seq Cs -> Int
strongest bs = seqFoldl (\acc x -> max (strength x) acc) 0 bs

longest :: Seq.Seq Cs -> Seq.Seq Cs
longest bs = seqFoldl f (Seq.singleton Seq.empty) bs where
  f acc x | Seq.length x > Seq.length (seqHead acc) = Seq.singleton x
          | Seq.length x == Seq.length (seqHead acc) = x Seq.<| acc
          | otherwise = acc

run :: IO ()
run = do
  inputRaw <- readFile "data/24.txt"
  putStrLn "example: "
  let exampleBridges = (validBridges' (parseInput exampleComponentsRaw))
  -- mapM_ (putStrLn . show) exampleBridges
  putStrLn $ "  max strength: " ++ (show (strongest exampleBridges))
  putStrLn $ "  longest strongest: " ++ (show (strongest . longest $ exampleBridges))
  putStrLn ""
  putStrLn "part1: "
  let p1Bridges = (validBridges' (parseInput inputRaw))
  -- mapM_ (putStrLn . show) p1Bridges
  putStrLn $ "  max strength: " ++ (show (strongest p1Bridges))
  putStrLn $ "  longest strongest: " ++ (show (strongest . longest $ p1Bridges))
