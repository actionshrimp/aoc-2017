{-# LANGUAGE NamedFieldPuns, ScopedTypeVariables, LambdaCase, MultiWayIf, RankNTypes #-}
module Sol25
    (run
    ) where

import Data.Maybe (fromMaybe)
import qualified Data.Map as M
import qualified Data.HashTable.IO as H
import Control.Monad.ST (ST, stToIO)
import Control.Monad (foldM)
import qualified Data.HashTable.ST.Basic as HB

data State = A|B|C|D|E|F
data Dir = L|R

data Env = Env
  { tape :: H.BasicHashTable Int Int
  , state :: State
  , slot :: Int
  }

val :: Env -> IO Int
val (Env { tape, slot }) = do
  v <- H.lookup tape slot
  return $ fromMaybe 0 v

next :: Env -> Int -> Dir -> State -> IO Env
next e@(Env {slot, tape}) v d s = do
  case v of
    0 -> H.delete tape slot
    1 -> H.insert tape slot 1

  return $ e
    { state = s
    , slot = case d of
        L -> slot - 1
        R -> slot + 1
    }

step :: Env -> IO Env
step  e@(Env { state })= do
  v <- val e
  case (state, v) of
    (A, 0) -> next e 1 R B
    (A, 1) -> next e 0 L B
    (B, 0) -> next e 1 L C
    (B, 1) -> next e 0 R E
    (C, 0) -> next e 1 R E
    (C, 1) -> next e 0 L D
    (D, 0) -> next e 1 L A
    (D, 1) -> next e 1 L A
    (E, 0) -> next e 0 R A
    (E, 1) -> next e 0 R F
    (F, 0) -> next e 1 R E
    (F, 1) -> next e 1 R A

checksum :: Env -> IO Int
checksum (Env { tape }) = H.foldM (\c _ -> return $ c+1) 0 tape

initialEnv :: IO Env
initialEnv = do
  t <- H.new
  return $ Env { tape = t, state = A, slot = 0}

envAfter :: Int -> Env -> IO Env
envAfter n e = do
  foldM (\e _ -> step e) e [1..n]

run :: IO ()
run = do
  let c = 12861455
  e <- initialEnv
  finalEnv <- (envAfter c e)
  result <- checksum finalEnv
  putStrLn $ "checksum after " ++ (show c) ++ ": " ++ (show result)

