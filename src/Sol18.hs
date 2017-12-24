{-# LANGUAGE NamedFieldPuns #-}
module Sol18
    (run
    ) where

import qualified Data.Map as M
import Data.Maybe (fromMaybe, catMaybes)
import Data.List.Split (splitOn)

type Reg = Char
type Val = Int

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
  { regs :: M.Map Reg Val
  , sound :: Maybe Val
  , rcvds :: [Val]
  , prog :: [Instruction]
  , i :: Int
  }

val :: Env -> Target -> Val
val e (TReg r) = fromMaybe 0 $ M.lookup r (regs e)
val e (TVal v) = v

inc :: Env -> Int -> Env
inc e@(Env { i, prog }) j = e { i = (i + j) `rem` (length prog) }

runInst :: Env -> Instruction -> Env
runInst e@(Env { i }) (Snd t) = (inc e 1) { sound = Just (val e t) }
runInst e@(Env { i, regs }) (Set r t) = (inc e 1) { regs = M.insert r (val e t) regs  }
runInst e@(Env { i, regs }) (Add r t) = (inc e 1) { regs = M.adjust ((+) (val e t)) r regs  }
runInst e@(Env { i, regs }) (Mul r t) = (inc e 1) { regs = M.adjust ((*) (val e t)) r regs  }
runInst e@(Env { i, regs }) (Mod r t) = (inc e 1) { regs = M.adjust ((flip rem) (val e t)) r regs  }
runInst e@(Env { i, sound, rcvds }) (Rcv t) = if (val e t) /= 0
                                              then (inc e 1) { rcvds = rcvds ++ (catMaybes [sound]) }
                                              else (inc e 1)
runInst e@(Env { i, regs }) (Jgz tc tv) = if (val e tc) > 0
                                          then (inc e (val e tv))
                                          else (inc e 1)

mkEnv :: [Instruction] -> Env
mkEnv insts = Env
  { regs = M.empty
  , sound = Nothing
  , rcvds = []
  , prog = insts
  , i = 0
  }

toFirstReceive :: [Instruction] -> Int
toFirstReceive insts = let
  e = mkEnv insts
  go env = let
    inst = (prog env) !! (i env)
    env' = runInst env inst
    rs = (rcvds env') in
    if (length rs) > 0 then (head rs) else go env'
  in go e

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
  putStrLn $ "example: " ++ (show (toFirstReceive exampleInstrs))
  inputRaw <- readFile "data/18.txt"
  let input = parseInput inputRaw
  putStrLn $ "part1: " ++ (show (toFirstReceive input))
