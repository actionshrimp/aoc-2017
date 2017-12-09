module Sol07
    (sol07
    ) where

import Text.Parsec
import Text.Parsec.String

data Mapping =
  Mapping { name :: String, weight :: Int, supports :: [String] }
  deriving Show

supportsParser :: Parser [String]
supportsParser = do
  s <- many1 letter
  rest <- ((do
    string ", "
    supportsParser) <|> (return []))
  return (s : rest)

mappingParser :: Parser Mapping
mappingParser = do
  n <- many1 letter
  string " ("
  w <- many1 digit
  string ")"
  s <- (do
    string " -> "
    supportsParser) <|> (return [])
  newline
  return $ Mapping { name = n, weight = (read w :: Int), supports = s }

parseInput :: String -> String -> Either ParseError [Mapping]
parseInput fname input = parse (many mappingParser) fname input

sol07 :: IO ()
sol07 = let fname = "data/07.txt" in do
  input <- readFile fname
  putStrLn (show (fmap last (parseInput fname input)))

