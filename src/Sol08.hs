{-# LANGUAGE NamedFieldPuns #-}
module Sol08
    (run
    ) where

import Text.Parsec
import Text.Parsec.String
import Debug.Trace (trace)
import Data.Maybe (fromMaybe)
import qualified Data.Map as M
import Data.List (maximumBy)
import Data.Ord (comparing)
import Control.Monad.Writer

type Op = (Int -> Int -> Int)
type Comp = (Int -> Int -> Bool)

data Inst
  = Inst { reg :: String
         , op :: Op
         , val :: Int
         , compReg :: String
         , comp :: Comp
         , compVal :: Int
         }

parseOp :: Parser Op
parseOp =
  (string "inc" >> (return (+))) <|>
  (string "dec" >> (return (-)))

parseComp :: Parser Comp
parseComp = do
  s <- many1 (oneOf "<>!=")
  return $
    case s of
      "<=" -> (<=)
      ">=" -> (>=)
      "==" -> (==)
      "!=" -> (/=)
      ">"  -> (>)
      "<"  -> (<)

parseInt :: Parser Int
parseInt = do
  s <- many1 (char '-' <|> digit)
  return $ read s

-- vyo inc -735 if hnh > -7
-- bz dec -959 if gx < 9
parseInst :: Parser Inst
parseInst = do
  reg <- many1 letter
  char ' '
  op <- parseOp
  char ' '
  val <- parseInt
  string " if "
  compReg <- many1 letter
  char ' '
  comp <- parseComp
  char ' '
  compVal <- parseInt
  newline
  return $ Inst {
    reg = reg, op = op, val = val,
    compReg = compReg, comp = comp, compVal = compVal
    }

type Env = M.Map String Int

runInst :: (Monoid w, MonadWriter w m) => Env -> Inst -> m Env
runInst e (Inst{reg, op, val, compReg, comp, compVal}) =
  if (comp (fromMaybe 0 (M.lookup compReg e)) compVal)
  then return $ M.alter (\x -> Just (op (fromMaybe 0 x) val)) reg e
  else return e

part1 :: [Inst] -> (String, Int)
part1 insts = let
  final = foldM runInst M.empty insts
  (result, _) = runWriter final :: (Env, [String])
  in maximumBy (comparing snd) (M.toList result)

parseInput :: String -> String -> Either ParseError [Inst]
parseInput fname input = parse (many parseInst) fname input

run :: IO ()
run = let fname = "data/08.txt" in do
  input <- readFile fname
  putStrLn (show (fmap part1 (parseInput fname input)))
