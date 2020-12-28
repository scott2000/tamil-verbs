-- | Conjugate verbs interactively through a REPL-like prompt
module Interactive where

import TamilString
import Verb
import Request

import Control.Monad
import Control.Exception

import System.IO
import System.Exit

import Data.List
import Data.Char

import qualified Data.Set as Set
import qualified Data.HashMap.Strict as HashMap

-- | Export a 'VerbList' to a given file path
exportVerbList :: FilePath -> VerbList -> IO ()
exportVerbList filePath verbList =
  tryWrite verbList `catch` \e ->
    putStrLn $ "cannot export verb list: " ++ show (e :: IOException)
  where
    tryWrite =
      writeFile filePath . unlines . map show . Set.toAscList . allVerbs

-- | Get a line from stdin if there is one, otherwise exit
getLineOrExit :: IO String
getLineOrExit =
  getLine `catch` (const exitSuccess :: IOException -> IO String)

-- | Start interactive mode, allowing the user to make multiple conjugation queries
startInteractive :: VerbList -> IO ()
startInteractive verbList = do
  let count list = show $ length $ allVerbs list
  putStr "\n> "
  hFlush stdout
  words <$> getLineOrExit >>= \case
    [] -> return ()
    ":parse":words -> do
      let
        isWordChar c = isAlpha c || isMark c || c == '~'
        skipSpecial [] = ("", "", "")
        skipSpecial str =
          let
            (special, specialRest) = span (not . isWordChar) str
            (alpha, alphaRest) = span isWordChar specialRest
            (t, l, e) =
              case parseTamil alpha of
                Left err ->
                  (alpha, alpha, "error: " ++ err ++ "\n")
                Right word ->
                  let
                    err =
                      case validateTamil word of
                        Left err -> "warning: " ++ err ++ "\n"
                        Right () -> ""
                  in
                    (toTamil word, toLatin word, err)
            (ts, ls, es) = skipSpecial alphaRest
          in
            (special ++ t ++ ts, special ++ l ++ ls, e ++ es)
        (t, l, e) = skipSpecial $ unwords words
      putStr e
      when (t /= l)
        if length l < 40 then
          putStrLn $ t ++ " (" ++ l ++ ")"
        else do
          putStrLn t
          putStrLn l
      startInteractive verbList
    ":guess":words -> do
      case mapM parseAndValidateTamil words of
        Left err ->
          putStrLn $ "error: " ++ err
        Right words ->
          let word = foldl' suffix (TamilString []) words in
          mapM_ (print . snd) $ guess True verbList word
      startInteractive verbList
    ":export":pathParts -> do
      let path = unwords pathParts
      exportVerbList path verbList
      putStrLn $ "exported " ++ count verbList ++ " verbs to '" ++ path ++ "'"
      startInteractive verbList
    ":load":pathParts -> do
      let
        path = unwords pathParts
        loadVerbList "" = return defaultVerbList
        loadVerbList filePath = do
          (errs, verbList) <- parseAllVerbs <$> readFile filePath
          mapM_ putStrLn errs
          putStrLn $ "loaded '" ++ path ++ "' (" ++ count verbList ++ " verbs)"
          return verbList
      verbList <- loadVerbList path `catch` \e -> do
        putStrLn $ "cannot read verb list: " ++ show (e :: IOException)
        return defaultVerbList
      startInteractive verbList
    ":add":verbParts ->
      case parseVerb (unwords verbParts) of
        Left err -> do
          putStrLn $ "syntax error: " ++ err
          startInteractive verbList
        Right verb -> do
          let verbList' = addVerb verb verbList
          putStrLn $ "verb added (" ++ count verbList' ++ " verbs)"
          startInteractive verbList'
    ":list":"def":rest -> do
      -- List by definition instead of just giving the verb list
      let min = case rest of "dup" : _ -> 1
                             _         -> 0
      forM_ (sort $ HashMap.toList $ byDefinition verbList) \(def, verbs) ->
        when (length verbs > min) $
          putStrLn $
            "[" ++ show (length verbs) ++ "] "
            ++ def ++ ": "
            ++ intercalate ", " (map getFormattedRoot $ Set.toList verbs)
      startInteractive verbList
    ":list":_ -> do
      mapM_ print $ Set.toAscList $ allVerbs verbList
      startInteractive verbList
    ":clear":_ -> do
      putStrLn "cleared all loaded verbs"
      startInteractive emptyVerbList
    ":reset":_ -> do
      putStrLn $ "reset to default verb list (" ++ count defaultVerbList ++ " verbs)"
      startInteractive defaultVerbList
    ":help":_ -> do
      putStr $ unlines
        [ ":parse <word>   parse the Tamil word and convert it to both alphabets"
        , ":guess <word>   guess an appropriate verb entry for this word"
        , ":export <file>  export the currently loaded verbs to a file"
        , ":load <file>    load a verb list from a file (replacing any loaded verbs)"
        , ":add <verb>     add a single verb"
        , ":list           list all loaded verbs"
        , ":clear          clear all loaded verbs"
        , ":reset          reset to the built-in verb list"
        , ":help           print this message"
        , ":quit           quit the program" ]
      startInteractive verbList
    ":quit":_ -> return ()
    ":q":_ -> return ()
    ":":_ ->
      startInteractive verbList
    (':':command):_ -> do
      putStrLn $ "unknown command :" ++ command
      startInteractive verbList
    verb:conjugation -> do
      processRequest verbList verb $ unwords conjugation
      startInteractive verbList
