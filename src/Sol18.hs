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
  deriving Show

data Instruction
  = Snd Target
  | Set Reg Target
  | Add Reg Target
  | Mul Reg Target
  | Mod Reg Target
  | Rcv Target
  | Jgz Target Target
  deriving Show


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
  e' = (inc e 1) in
  case inst of
    (Snd t)     -> runPart1 (e', Just (val e t))
    (Set r t)   -> runPart1 (e' { regs = M.insert r (val e t) regs }, s)
    (Add r t)   -> runPart1 (e' { regs = M.adjust ((+) (val e t)) r regs }, s)
    (Mul r t)   -> runPart1 (e' { regs = M.adjust ((*) (val e t)) r regs }, s)
    (Mod r t)   -> runPart1 (e' { regs = M.adjust ((flip rem) (val e t)) r regs }, s)
    (Rcv t)     -> if (val e t) /= 0 then s else runPart1 (e', s)
    (Jgz tc tv) -> runPart1 (if (val e tc) > 0 then (inc e (val e tv)) else e', s)

mkEnv :: [Instruction] -> [(Char, Int)] -> Env
mkEnv insts initRegs = Env
  { regs = M.fromList initRegs
  , prog = insts
  , i = 0
  }

part1 :: [Instruction] -> Maybe Int
part1 insts = let
  e = mkEnv insts [] in
  runPart1 (e, Nothing)

runPart2 :: (Env, [Int], [Int]) -> (Env, [Int], [Int])
runPart2 (e@(Env { i, regs, prog }), toRcv, toSnd) = let
  inst = prog !! i
  e' = (inc e 1) in
  case inst of
    (Snd t) -> runPart2 (e', toRcv, toSnd ++ [(val e t)])
    (Set r t) -> runPart2 (e' { regs = M.insert r (val e t) regs }, toRcv, toSnd)
    (Add r t) -> runPart2 (e' { regs = M.adjust ((+) (val e t)) r regs }, toRcv, toSnd)
    (Mul r t) -> runPart2 (e' { regs = M.adjust ((*) (val e t)) r regs }, toRcv, toSnd)
    (Mod r t) -> runPart2 (e' { regs = M.adjust ((flip rem) (val e t)) r regs }, toRcv, toSnd)
    (Rcv (TReg r)) ->
      case toRcv of
        [] -> (e, [], toSnd)
        rcv:rcvs -> runPart2 (e' { regs = M.insert r rcv regs }, rcvs, toSnd)
    (Jgz tc tv) -> runPart2 (if (val e tc) > 0 then (inc e (val e tv)) else e', toRcv, toSnd)

part2 :: [Instruction] -> Int
part2 insts = let
  e0 = mkEnv insts [('p', 0)]
  e1 = mkEnv insts [('p', 1)]
  go x0 x1 c = let
      (e0', r0, s0) = runPart2 x0
      (e1', r1, s1) = runPart2 x1
      c' = (c + length s1)
    in do
      case (s0, s1) of
        ([], []) -> c'
        _ -> go (e0', r0 ++ s1, []) (e1', r1 ++ s0, []) c'

  in go (e0, [], []) (e1, [], []) 0

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

exampleInstrsPt2Raw = "\
\snd 1\n\
\snd 2\n\
\snd p\n\
\rcv a\n\
\rcv b\n\
\rcv c\n\
\rcv d"

exampleInstrsPt2 = parseInput exampleInstrsPt2Raw

run :: IO ()
run = do
  putStrLn $ "example1: " ++ (show (part1 exampleInstrs))
  inputRaw <- readFile "data/18.txt"
  let input = parseInput inputRaw
  putStrLn $ "part1: " ++ (show (part1 input))
  putStrLn $ "example2: " ++ (show (part2 exampleInstrsPt2))
  putStrLn $ "part2: " ++ (show (part2 input))
