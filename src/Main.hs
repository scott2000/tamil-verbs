module Main where

import Request

main :: IO ()
main = do
  putStrLn "Enter a word and a way to conjugate it.\n"
  go
  where
    go = words <$> getLine >>= \case
      [] -> return ()
      (verb:conjugation) -> do
        processRequest verb $ unwords conjugation
        putStrLn ""
        go

