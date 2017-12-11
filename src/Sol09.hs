{-# LANGUAGE NamedFieldPuns #-}
module Sol09
    (run
    ) where

data ExecSt
  = ExecSt { depth :: Int
           , score :: Int
           , cancelNext :: Bool
           , garbage :: Bool
           , gCount :: Int
           }
  deriving Show

parseStream :: ExecSt -> Char -> ExecSt
parseStream s@(ExecSt { cancelNext = False }) '!' = s{ cancelNext = True }
parseStream s@(ExecSt { cancelNext = True }) _ = s{ cancelNext = False }
parseStream s@(ExecSt { cancelNext = False, garbage = False }) '<' = s{ garbage = True }
parseStream s@(ExecSt { cancelNext = False, garbage = True }) '>' = s{ garbage = False }
parseStream s@(ExecSt { cancelNext = False, garbage = True, gCount })  _ = s{ gCount = gCount + 1 }
parseStream s@(ExecSt { depth, score }) '{' = s{ depth = depth + 1 }
parseStream s@(ExecSt { depth, score }) '}' = s{ depth = depth - 1, score = score + depth }
parseStream s  _ = s

initState = ExecSt { depth = 0
                    , score = 0
                    , cancelNext = False
                    , garbage = False
                    , gCount = 0
                    }

runParse :: String -> ExecSt
runParse s =
  foldl parseStream initState s

run :: IO ()
run = let fname = "data/09.txt" in do
  input <- readFile fname
  putStrLn (show (runParse input))
  putStrLn $ "part 1: " ++ (show (score (runParse input)))
  putStrLn $ "part 2: " ++ (show (gCount (runParse input)))
