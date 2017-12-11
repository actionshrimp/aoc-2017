{-# LANGUAGE NamedFieldPuns #-}
module Sol09
    (run
    ) where

data ExecSt
  = ExecSt { depth :: Int
           , score :: Int
           , cancelNext :: Bool
           , garbage :: Bool
           }
  deriving Show

parseStream :: ExecSt -> Char -> ExecSt
parseStream s@(ExecSt { cancelNext = False }) '!' = s{ cancelNext = True }
parseStream s@(ExecSt { cancelNext = True }) _ = s{ cancelNext = False }
parseStream s@(ExecSt { cancelNext = False, garbage }) '<' = s{ garbage = True }
parseStream s@(ExecSt { cancelNext = False, garbage = True }) '>' = s{ garbage = False }
parseStream s@(ExecSt { cancelNext = False, garbage = True })  _ = s
parseStream s@(ExecSt { depth, score }) '{' = s{ depth = depth + 1 }
parseStream s@(ExecSt { depth, score }) '}' = s{ depth = depth - 1, score = score + depth }
parseStream s  _ = s

initState = ExecSt { depth = 0
                    , score = 0
                    , cancelNext = False
                    , garbage = False }

run :: IO ()
run = let fname = "data/09.txt" in do
  input <- readFile fname
  putStrLn (show (foldl parseStream initState input))
