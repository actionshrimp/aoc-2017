{-# LANGUAGE NamedFieldPuns, ScopedTypeVariables, LambdaCase, MultiWayIf #-}
module Sol25
    (run
    ) where

import Data.Maybe (fromMaybe)
import qualified Data.Map as M

data State = A|B|C|D|E|F
data Dir = L|R

data Env = Env
  { tape :: M.Map Int Int
  , state :: State
  , slot :: Int
  }

val :: Env -> Int
val (Env { tape, slot }) = fromMaybe 0 $ M.lookup slot tape

next :: Env -> Int -> Dir -> State -> Env
next (Env {slot, tape}) v d s = Env
  { tape = case v of
      0 -> M.delete slot tape
      1 -> M.insert slot 1 tape
  , state = s
  , slot = case d of
      L -> slot - 1
      R -> slot + 1
  }

step :: Env -> Env
step e@(Env { state = A }) | val e == 0 = next e 1 R B
step e@(Env { state = A }) | val e == 1 = next e 0 L B
step e@(Env { state = B }) | val e == 0 = next e 1 L C
step e@(Env { state = B }) | val e == 1 = next e 0 R E
step e@(Env { state = C }) | val e == 0 = next e 1 R E
step e@(Env { state = C }) | val e == 1 = next e 0 L D
step e@(Env { state = D }) | val e == 0 = next e 1 L A
step e@(Env { state = D }) | val e == 1 = next e 1 L A
step e@(Env { state = E }) | val e == 0 = next e 0 R A
step e@(Env { state = E }) | val e == 1 = next e 0 R F
step e@(Env { state = F }) | val e == 0 = next e 1 R E
step e@(Env { state = F }) | val e == 1 = next e 1 R A

checksum :: Env -> Int
checksum (Env { tape }) = M.size tape

initialEnv :: Env
initialEnv = Env { tape = M.empty, state = A, slot = 0}

after :: Int -> Env -> Env
after n e = head $ drop n $ iterate step e

run :: IO ()
run = do
  let c = 12861455
  let finalEnv = after c initialEnv
  putStrLn $ "checksum after " ++ (show c) ++ ": " ++ (show (checksum finalEnv))

