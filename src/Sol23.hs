{-# LANGUAGE NamedFieldPuns, ScopedTypeVariables, LambdaCase, MultiWayIf #-}
module Sol23
    (run
    ) where

import Data.List.Split (splitOn)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)

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

run :: IO ()
run = do
  inputRaw <- readFile "data/23.txt"
  let input = parseInput inputRaw
  putStrLn $ "part1: " ++ (show (runPart1 input (0, Env {i = 0, regs = M.empty})))
