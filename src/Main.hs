module Main where

import Verb
import Request

import System.IO
import System.Exit
import System.Environment

loadVerbList :: FilePath -> IO VerbList
loadVerbList filePath = do
  (errs, verbList) <- parseAllVerbs <$> readFile filePath
  mapM_ putStrLn errs
  return verbList

main :: IO ()
main = do
  verbList <-
    getArgs >>= \case
      [] ->
        return defaultVerbList
      [filePath] ->
        loadVerbList filePath
      _ -> do
        putStrLn "expected either no arguments or a single argument with a path to a verb list"
        exitFailure
  putStrLn "Enter a word and a way to conjugate it.\n"
  startInteractive verbList
  where
    startInteractive verbList = do
      putStr "> "
      hFlush stdout
      words <$> getLine >>= \case
        [] -> return ()
        (":load":filePath) ->
          loadVerbList (unwords filePath) >>= startInteractive
        (verb:conjugation) -> do
          processRequest verbList verb $ unwords conjugation
          putStrLn ""
          startInteractive verbList

