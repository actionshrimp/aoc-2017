module Sol07
    (run
    ) where

import Text.Parsec
import Text.Parsec.String
import qualified Data.Set as S
import qualified Data.Map as M
import Data.Maybe (fromMaybe, catMaybes)
import Data.List (elemIndex, sortBy, maximumBy)
import Data.Ord (comparing)

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

part1 :: [Mapping] -> String
part1 ms = let
  names = S.fromList $ map name ms
  refs = S.fromList $ concatMap supports ms
  in
  head $ S.toList (S.difference names refs)

byName :: [Mapping] -> M.Map String Mapping
byName ms = foldl (\d m@(Mapping { name = n }) -> M.insert n m d) M.empty ms

totalWeight :: M.Map String Mapping -> String -> Maybe Int
totalWeight bn n = do
  current <- M.lookup n bn
  children <- mapM (\x -> M.lookup x bn) (supports current)
  childWeights <- mapM (totalWeight bn . name) children
  return $ (weight current) + (foldl (+) 0 childWeights)

tooHeavyChild :: M.Map String Mapping -> String -> Maybe String
tooHeavyChild bn n = do
  current <- M.lookup n bn
  children <- mapM (\x -> M.lookup x bn) (supports current)
  childWeights <- mapM (totalWeight bn . name) children
  case all (== (head childWeights)) childWeights of
    True -> Nothing
    False -> let
      tooHeavyIdx = fst $ maximumBy (comparing snd) (zip [0..] childWeights)
      in Just (name (children !! tooHeavyIdx))

solvePart2 :: M.Map String Mapping -> String -> Maybe (String, Int, Int, Int, Int)
solvePart2 bn n = do
  current <- M.lookup n bn
  hName <- tooHeavyChild bn (name current)
  case tooHeavyChild bn hName of
    Nothing -> do
      expectedTotal <- (totalWeight bn) . head . (filter (/= hName)) $ (supports current)
      curTotal <- totalWeight bn hName
      curW <- fmap weight $ M.lookup hName bn
      Just (hName, curTotal, expectedTotal, curW, curW - curTotal + expectedTotal)
    Just next -> solvePart2 bn hName

-- eugh
-- part2 :: [Mapping] -> Maybe Int
part2 ms = let
  root = part1 ms
  bn = byName ms
  in do
    (name, curTotal, expectedTotal, curW, expectedW) <- solvePart2 bn root
    return expectedW

run :: IO ()
run = let fname = "data/07.txt" in do
  input <- readFile fname
  putStrLn (show (fmap part2 (parseInput fname input)))

