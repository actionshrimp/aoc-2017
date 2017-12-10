module Sol08
    (run
    ) where

import Text.Parsec
import Text.Parsec.String
import Debug.Trace (trace)

data Op = Inc | Dec
  deriving Show

data Comp = CLTE | CLT | CEQ | CNEQ | CGT | CGTE
  deriving Show

data Inst
  = Inst { reg :: String
         , op :: Op
         , val :: Int
         , compReg :: String
         , comp :: Comp
         , compVal :: Int
         }
  deriving Show

parseOp :: Parser Op
parseOp =
  (string "inc" >> (return Inc)) <|>
  (string "dec" >> (return Dec))

parseComp :: Parser Comp
parseComp = do
  s <- many1 (oneOf "<>!=")
  return $
    case s of
      "<=" -> CLTE
      ">=" -> CGTE
      "==" -> CEQ
      "!=" -> CNEQ
      ">"  -> CGT
      "<"  -> CLT

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

parseInput :: String -> String -> Either ParseError [Inst]
parseInput fname input = parse (many parseInst) fname input

run :: IO ()
run = let fname = "data/08.txt" in do
  input <- readFile fname
  putStrLn (show (fmap head (parseInput fname input)))
