{-# LANGUAGE NamedFieldPuns #-}
module Sol18
    (run
    ) where

import qualified Data.Map as M
import Data.Maybe (fromMaybe, catMaybes)
import Data.List.Split (splitOn)

type Reg = Char
type Val = Int

type Regs = M.Map Reg Val

data Target
  = TReg Reg
  | TVal Val

data Instruction
  = Snd Target
  | Set Reg Target
  | Add Reg Target
  | Mul Reg Target
  | Mod Reg Target
  | Rcv Target
  | Jgz Target Target


data Env = Env
  { regs :: Regs
  , prog :: [Instruction]
  , i :: Int
  }

val :: Env -> Target -> Val
val e (TReg r) = fromMaybe 0 $ M.lookup r (regs e)
val e (TVal v) = v

inc :: Env -> Int -> Env
inc e@(Env { i, prog }) j = e { i = (i + j) `rem` (length prog) }

runPart1 :: (Env, Maybe Int) -> Maybe Int
runPart1 (e@(Env { i, regs, prog }), s) = let
  inst = prog !! i
  e' = (inc e 1)
  in
  case inst of
    (Snd t)     -> runPart1 (e', Just (val e t))
    (Set r t)   -> runPart1 (e' { regs = M.insert r (val e t) regs }, s)
    (Add r t)   -> runPart1 (e' { regs = M.adjust ((+) (val e t)) r regs }, s)
    (Mul r t)   -> runPart1 (e' { regs = M.adjust ((*) (val e t)) r regs }, s)
    (Mod r t)   -> runPart1 (e' { regs = M.adjust ((flip rem) (val e t)) r regs }, s)
    (Rcv t)     -> if (val e t) /= 0 then s else runPart1 (e', s)
    (Jgz tc tv) -> runPart1 (if (val e tc) > 0 then (inc e (val e tv)) else e', s)

mkEnv :: [Instruction] -> Env
mkEnv insts = Env
  { regs = M.empty
  , prog = insts
  , i = 0
  }

part1 :: [Instruction] -> Maybe Int
part1 insts = let
  e = mkEnv insts in
  runPart1 (e, Nothing)

parseTarget :: String -> Target
parseTarget s | (head s) `elem` ['a'..'z'] = TReg (head s)
              | otherwise = TVal (read s)

parseLine :: [String] -> Instruction
parseLine ("snd":a:[]) = Snd (parseTarget a)
parseLine ("set":a:b:[]) = Set (head a) (parseTarget b)
parseLine ("add":a:b:[]) = Add (head a) (parseTarget b)
parseLine ("mul":a:b:[]) = Mul (head a) (parseTarget b)
parseLine ("mod":a:b:[]) = Mod (head a) (parseTarget b)
parseLine ("rcv":a:[]) = Rcv (parseTarget a)
parseLine ("jgz":a:b:[]) = Jgz (parseTarget a) (parseTarget b)

parseInput :: String -> [Instruction]
parseInput s = map (parseLine . splitOn " ") (lines s)

exampleInstrsRaw = "\
\set a 1\n\
\add a 2\n\
\mul a a\n\
\mod a 5\n\
\snd a\n\
\set a 0\n\
\rcv a\n\
\jgz a -1\n\
\set a 1\n\
\jgz a -2"

exampleInstrs = parseInput exampleInstrsRaw

run :: IO ()
run = do
  putStrLn $ "example: " ++ (show (part1 exampleInstrs))
  inputRaw <- readFile "data/18.txt"
  let input = parseInput inputRaw
  putStrLn $ "part1: " ++ (show (part1 input))
