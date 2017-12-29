{-# LANGUAGE NamedFieldPuns, ScopedTypeVariables, LambdaCase, MultiWayIf #-}
module Sol23
    (run
    ) where

import Data.List.Split (splitOn)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Data.IORef (IORef, newIORef, readIORef, writeIORef, modifyIORef)
import Control.Monad (when, unless)
import Control.Monad.Loops (untilM_)

type Reg = Char
type Val = Int

data Target
  = TReg Reg
  | TVal Val
  deriving Show

data Instruction
  = Set Reg Target
  | Sub Reg Target
  | Mul Reg Target
  | Jgz Target Target
  deriving Show

parseTarget :: String -> Target
parseTarget s | (head s) `elem` ['a'..'z'] = TReg (head s)
              | otherwise = TVal (read s)

parseLine :: [String] -> Instruction
parseLine ("set":a:b:[]) = Set (head a) (parseTarget b)
parseLine ("sub":a:b:[]) = Sub (head a) (parseTarget b)
parseLine ("mul":a:b:[]) = Mul (head a) (parseTarget b)
parseLine ("jnz":a:b:[]) = Jgz (parseTarget a) (parseTarget b)

parseInput :: String -> Prog
parseInput s = map (parseLine . splitOn " ") (lines s)

type Prog = [Instruction]

data Env = Env
  { i :: Int
  , regs :: M.Map Reg Val
  }

val :: Env -> Target -> Val
val e (TReg r) = fromMaybe 0 $ M.lookup r (regs e)
val e (TVal v) = v

inc :: Env -> Int -> Env
inc e@(Env { i }) j = e { i = (i + j) }

runPart1 :: Prog -> (Int, Env) -> Int
runPart1 p (mulCount, e@Env {i, regs}) =
  if i < 0 || i >= (length p) then mulCount
  else let
    e' = (inc e 1)
    in case p !! i of
         (Set r t) -> runPart1 p (mulCount, e' { regs = M.insert r (val e t) regs})
         (Sub r t) -> runPart1 p (mulCount, e' { regs = M.adjust (\x -> x - (val e t)) r regs})
         (Mul r t) -> runPart1 p (mulCount+1, e' { regs = M.adjust ((*) (val e t)) r regs})
         (Jgz tc tv) -> runPart1 p (mulCount, if (val e tc) /= 0 then (inc e (val e tv)) else e')

-- runPart2 :: Prog -> Env -> IO Env
-- runPart2 p (e@Env {i, regs}) = do
--   -- putStrLn ("b: " ++ (show (M.lookup 'b' regs)))
--   -- putStrLn ("d: " ++ (show (M.lookup 'd' regs)))
--   -- putStrLn ("hack: " ++ (show hack))
--   putStrLn (show i)
--   if i == 11 then do
--     putStrLn ("a: " ++ (show (M.lookup 'a' regs)))
--     putStrLn ("b: " ++ (show (M.lookup 'b' regs)))
--     putStrLn ("c: " ++ (show (M.lookup 'c' regs)))
--     putStrLn ("d: " ++ (show (M.lookup 'd' regs)))
--     putStrLn ("e: " ++ (show (M.lookup 'e' regs)))
--     putStrLn ("f: " ++ (show (M.lookup 'f' regs)))
--     putStrLn ("g: " ++ (show (M.lookup 'g' regs)))
--     putStrLn ("h: " ++ (show (M.lookup 'h' regs)))
--     else return ()

--   if i < 0 || i >= (length p) then return e
--   else let
--     e' = (inc e 1)
--     valOfB = fromMaybe (-1) (M.lookup 'b' regs)
--     in case (p !! i) of
--          (Set r t) -> runPart2 p (e' { regs = M.insert r (val e t) regs})
--          (Sub r t) -> runPart2 p (e' { regs = M.adjust (\x -> x - (val e t)) r regs})
--          (Mul r t) -> runPart2 p (e' { regs = M.adjust ((*) (val e t)) r regs})
--          (Jgz tc tv) -> runPart2 p (if (val e tc) /= 0 then (inc e (val e tv)) else e')


-- set b 81
-- set c b
-- if (debug) {
--   mul b 100
--   sub b -100000
--   set c b
--   sub c -17000
-- }
-- do {
--   set f 1
--   set d 2
--   do {
--     set e 2
--     do {
--       set g d
--       mul g e
--       sub g b
--       if (g == 0) {
--         set f 0
--       }
--       sub e -1
--       set g e
--       sub g b
--     } until (g == 0)

--     sub d -1
--     set g d
--     sub g b
--   } until (g == 0)

--   jnz f 2
--   sub h -1

--   set g b
--   sub g c
--   if (g == 0) { return; }
--   sub b -17
-- }

-- port of hand-jigged commented steps above
runPart2 :: Bool -> IO Int
runPart2 debug = do
  b <- newIORef 0
  c <- newIORef 0
  d <- newIORef 0
  e <- newIORef 0
  f <- newIORef 0
  g <- newIORef 0
  h <- newIORef 0
  writeIORef b 81
  writeIORef c 81

  unless debug $ do
      modifyIORef b (\x -> x * 100 + 100000)
      vb <- readIORef b
      writeIORef c (vb + 17000)

  let eq0 r = (fmap (\x -> x == 0) (readIORef r))

  (do
    vb <- readIORef b

    writeIORef f 1
    writeIORef d 2

    (do
      -- writeIORef e 2

      -- (do
      --   vd <- readIORef d
      --   ve <- readIORef e
      --   vb <- readIORef b
      --   writeIORef g (vd * ve - vb)

      --   vg <- readIORef g

      --   when (vg == 0) $ do
      --     writeIORef f 0

      --   modifyIORef e ((+) 1)
      --   ve <- readIORef e
      --   vb <- readIORef b
      --   writeIORef g (ve - vb)) `untilM_` geq0

      -- replaces inner loop above for speeds
      vd <- readIORef d
      vb <- readIORef b

      when (vb `rem` vd == 0) $
        writeIORef f 0
      -- end of replacement
      -- guessing there's some sort of prime number deely going on here

      modifyIORef d ((+) 1)
      vd <- readIORef d
      vb <- readIORef b
      writeIORef g (vd - vb)) `untilM_` (eq0 g)

    vf <- readIORef f
    when (vf == 0) $ do
      modifyIORef h ((+) 1)

    vb <- readIORef b
    vc <- readIORef c
    writeIORef g (vb - vc)

    vg <- readIORef g

    modifyIORef b ((+) 17)) `untilM_` (eq0 g)

  vh <- readIORef h
  return $ vh

run :: IO ()
run = do
  inputRaw <- readFile "data/23.txt"
  let input = parseInput inputRaw
  putStrLn $ "part1: " ++ (show (runPart1 input (0, Env {i = 0, regs = M.empty})))
  p2debug <- runPart2 True
  putStrLn $ "part2 (debug on): " ++ (show p2debug)
  p2 <- runPart2 False
  putStrLn $ "part2 (debug off): " ++ (show p2)
