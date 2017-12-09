module Sol07
    (sol07
    ) where

import Text.Parsec
import Text.Parsec.String
import qualified Data.Set as S

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

nameOfRoot :: [Mapping] -> String
nameOfRoot ms = let
  names = S.fromList $ map name ms
  refs = S.fromList $ concatMap supports ms
  in
  head $ S.toList (S.difference names refs)

sol07 :: IO ()
sol07 = let fname = "data/07.txt" in do
  input <- readFile fname
  putStrLn (show (fmap nameOfRoot (parseInput fname input)))

