{-# LANGUAGE NamedFieldPuns #-}
module Sol20
    (run
    ) where

import Data.List.Split (splitOn)
import Data.List (minimumBy, sortBy)
import Data.Ord (comparing)
import Text.Regex.Posix

data Particle = Particle
  { p :: (Int, Int, Int)
  , v :: (Int, Int, Int)
  , a :: (Int, Int, Int)
  } deriving Show

parseItem :: String -> (Char, (Int, Int, Int))
parseItem s = let
  (_, _, _, gs) = s =~ "(.)=<(.*),(.*),(.*)>" :: (String, String, String, [String])
  c:x:y:z:[] = gs
  in ((head c), ((read x), (read y), (read z)))

parseLine :: String -> Particle
parseLine l = let
  p:v:a:[] = splitOn ", " l
  (_, ps) = parseItem p
  (_, vs) = parseItem v
  (_, as) = parseItem a
  in Particle { p = ps, v = vs, a = as }

magA :: Particle -> Int
magA p@(Particle {a = (ax, ay, az)}) = (abs ax) + (abs ay) + (abs az)

magV :: Particle -> Int
magV p@(Particle {v = (vx, vy, vz)}) = (abs vx) + (abs vy) + (abs vz)

magP :: Particle -> Int
magP p@(Particle {p = (px, py, pz)}) = (abs px) + (abs py) + (abs pz)

part1 :: [Particle] -> [(Int, Particle)]
part1 ps = let
  ided = zip [0..] ps
  in sortBy (comparing (magA . snd) `mappend` comparing (magV . snd) `mappend` comparing (magP . snd) ) ided

run :: IO ()
run = do
  input <- readFile "data/20.txt"
  let particles = map parseLine $ lines input
  -- It's gonna be one of the ones with shortest acc modulo starting deltas
  putStrLn $ "part1: " ++ (show (take 5 (part1 particles)))
