{-# LANGUAGE NamedFieldPuns #-}
module Sol18
    (run
    ) where

import qualified Data.Map as M
import Data.Maybe (fromMaybe, catMaybes)

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

mov :: Env -> Int -> Env
mov e@(Env { i, prog }) j = e { i = (i + j) `rem` (length prog) }

runInst :: Env -> Instruction -> Env
runInst e@(Env { i }) (Snd t) = (mov e 1) { sound = Just (val e t) }
runInst e@(Env { i, regs }) (Set r t) = (mov e 1) { regs = M.insert r (val e t) regs  }
runInst e@(Env { i, regs }) (Add r t) = (mov e 1) { regs = M.adjust ((+) (val e t)) r regs  }
runInst e@(Env { i, regs }) (Mul r t) = (mov e 1) { regs = M.adjust ((*) (val e t)) r regs  }
runInst e@(Env { i, regs }) (Mod r t) = (mov e 1) { regs = M.adjust ((flip rem) (val e t)) r regs  }
runInst e@(Env { i, sound, rcvds }) (Rcv t) = if (val e t) /= 0
                                              then (mov e 1) { rcvds = rcvds ++ (catMaybes [sound]) }
                                              else (mov e 1)
runInst e@(Env { i, regs }) (Jgz tc tv) = if (val e tc) > 0
                                          then (mov e (val e tv))
                                          else (mov e 1)

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

exampleInstrs =
  [ Set 'a' (TVal 1)
  , Add 'a' (TVal 2)
  , Mul 'a' (TReg 'a')
  , Mod 'a' (TVal 5)
  , Snd (TReg 'a')
  , Set 'a' (TVal 0)
  , Rcv (TReg 'a')
  , Jgz (TReg 'a') (TVal (-1))
  , Set 'a' (TVal 1)
  , Jgz (TReg 'a') (TVal (-2))
  ]

run :: IO ()
run = do
  putStrLn $ "example: " ++ (show (toFirstReceive exampleInstrs))
