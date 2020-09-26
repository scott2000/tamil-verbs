module Main where

import TamilString
import Verb
import Request

import System.IO
import System.Exit
import System.Environment

import Data.List
import Data.Char

import qualified Data.Set as Set

loadVerbList :: FilePath -> IO VerbList
loadVerbList filePath = do
  (errs, verbList) <- parseAllVerbs <$> readFile filePath
  mapM_ putStrLn errs
  return verbList

exportVerbList :: FilePath -> VerbList -> IO ()
exportVerbList filePath =
  writeFile filePath . unlines . map show . Set.toAscList . allVerbs

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
  putStrLn "Enter a word and a way to conjugate it."
  startInteractive verbList
  where
    startInteractive verbList = do
      let count list = show $ length $ allVerbs list
      putStr "\n> "
      hFlush stdout
      words <$> getLine >>= \case
        [] -> return ()
        (":parse":words) -> do
          let
            isWordChar c = isAlpha c || isMark c
            skipSpecial [] = ("", "")
            skipSpecial str =
              let
                (special, specialRest) = span (not . isWordChar) str
                (alpha, alphaRest) = span isWordChar specialRest
                (t, l) =
                  case parseTamil alpha of
                    Left _ ->
                      (alpha, alpha)
                    Right word ->
                      (toTamil word, toLatin word)
                (ts, ls) = skipSpecial alphaRest
              in
                (special ++ t ++ ts, special ++ l ++ ls)
            (t, l) = skipSpecial $ unwords words
          if t == l then
            putStrLn "error: no valid Tamil words found"
          else if length l < 40 then
            putStrLn $ t ++ " (" ++ l ++ ")"
          else do
            putStrLn t
            putStrLn l
          startInteractive verbList
        (":guess":words) -> do
          case mapM parseAndValidateTamil words of
            Left err ->
              putStrLn $ "error: " ++ err
            Right words ->
              let word = foldl' suffix (TamilString []) words in
              case guess verbList word of
                [] ->
                  putStrLn "error: could not guess word"
                verbs ->
                  mapM_ (print . snd) verbs
          startInteractive verbList
        (":export":pathParts) -> do
          let path = unwords pathParts
          exportVerbList path verbList
          putStrLn $ "exported " ++ count verbList ++ " verbs to '" ++ path ++ "'"
          startInteractive verbList
        (":load":pathParts) -> do
          let path = unwords pathParts
          verbList <- loadVerbList path
          putStrLn $ "loaded '" ++ path ++ "' (" ++ count verbList ++ " verbs)"
          startInteractive verbList
        (":add":verbParts) ->
          case parseVerb (unwords verbParts) of
            Left err -> do
              putStrLn $ "syntax error: " ++ err
              startInteractive verbList
            Right verb -> do
              let verbList' = addVerb verb verbList
              putStrLn $ "verb added (" ++ count verbList' ++ " verbs)"
              startInteractive verbList'
        (":list":_) -> do
          mapM_ print $ Set.toAscList $ allVerbs verbList
          startInteractive verbList
        (":clear":_) -> do
          putStrLn "cleared all loaded verbs"
          startInteractive emptyVerbList
        (":reset":_) -> do
          putStrLn $ "reset to default verb list (" ++ count defaultVerbList ++ " verbs)"
          startInteractive defaultVerbList
        (":help":_) -> do
          putStrLn ":parse <word>   parse the Tamil word and convert it to both alphabets"
          putStrLn ":guess <word>   guess an appropriate verb entry for this word"
          putStrLn ":export <file>  export the currently loaded verbs to a file"
          putStrLn ":load <file>    load a verb list from a file (replacing any loaded verbs)"
          putStrLn ":add <verb>     add a single verb"
          putStrLn ":list           list all loaded verbs"
          putStrLn ":clear          clear all loaded verbs"
          putStrLn ":reset          reset to the default verb list"
          putStrLn ":help           print this message"
          putStrLn ":quit           quit the program"
          startInteractive verbList
        (":quit":_) -> return ()
        (":q":_) -> return ()
        (":":_) ->
          startInteractive verbList
        ((':':command):_) -> do
          putStrLn $ "unknown command :" ++ command
          startInteractive verbList
        (verb:conjugation) -> do
          processRequest verbList verb $ unwords conjugation
          startInteractive verbList

