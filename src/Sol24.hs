{-# LANGUAGE NamedFieldPuns, ScopedTypeVariables, LambdaCase, MultiWayIf #-}
module Sol24
    (run
    ) where

import Data.List.Split (splitOn)
import Data.List (delete, nub, sort)

type Components = [(Int, Int)]

parseInput :: String -> Components
parseInput s = [(read a, read b) | l  <- lines s, let [a, b] = splitOn "/" l ]

toFixedPoint :: Eq a => (a -> a) -> a -> a
toFixedPoint f x = let y = f x in
  if y == x then x else toFixedPoint f y

expandValid :: [(Components, Components)] -> [(Components, Components)]
expandValid xs = nub $ xs ++ do
  x@(cs, b@((_, bp):bs)) <- xs
  c@(cpa, cpb) <- filter (\(cpa, cpb) -> cpa == bp || cpb == bp) cs
  return $ (delete c cs, if cpa == bp then c:b else (cpb, cpa):b)

validBridges :: Components -> [Components]
validBridges cs = delete [] $ map (init . snd) $ toFixedPoint expandValid [(cs, [(0, 0)])]

exampleComponentsRaw = "\
\0/2\n\
\2/2\n\
\2/3\n\
\3/4\n\
\3/5\n\
\0/1\n\
\10/1\n\
\9/10"

strength :: Components -> Int
strength cs = sum $ map (\(a, b) -> a+b) cs

run :: IO ()
run = do
  inputRaw <- readFile "data/24.txt"
  putStrLn "example: "
  let exampleBridges = (validBridges (parseInput exampleComponentsRaw))
  mapM_ (putStrLn . show) exampleBridges
  putStrLn $ "max strength: " ++ (show (head . reverse . sort $ map strength exampleBridges))
  putStrLn ""
  putStrLn "part1: "
  putStrLn inputRaw
  let p1Bridges = (validBridges (parseInput inputRaw))
  -- mapM_ (putStrLn . show) p1Bridges
  putStrLn $ "max strength: " ++ (show (head . reverse . sort $ map strength p1Bridges))
