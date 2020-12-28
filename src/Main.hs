-- | An program for conjugating Tamil verbs and generating quizzes
module Main where

import TamilString
import Verb
import Request
import Learn
import Interactive

import Control.Monad
import Control.Exception

import System.IO
import System.Exit
import System.Environment
import System.FilePath

import Text.Read

import qualified Data.Set as Set
import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap

-- | The kind of parameter an option accepts
data OptionKind
  = OptionFile    -- ^ A file path
  | OptionNumber  -- ^ An integer

instance Show OptionKind where
  show = \case
    OptionFile -> "a file path"
    OptionNumber -> "a number"

-- | The kind of an argument
data ArgKind
  = ArgFlag               -- ^ A flag which does not accept any parameters
  | ArgOption !OptionKind -- ^ An option which may accept a parameter

-- | A definition of what kinds of arguments are expected for a command
type ArgKinds = HashMap String ArgKind

-- | A set of parsed flags
type Flags = HashSet String

-- | A map of parsed flags to their parsed parameters
type Options = HashMap String String

-- | Remaining arguments that were not flags or options
type Arguments = [String]

-- | Given a description of possible arguments and a command name, parse a list of raw arguments
parseArgs :: ArgKinds -> String -> [String] -> IO (Flags, Options, Arguments, VerbList)
parseArgs kinds command args =
  go HashSet.empty HashMap.empty [] args
  where
    kinds' = kinds <> HashMap.fromList
      [ ("list", ArgOption OptionFile)
      , ("help", ArgFlag) ]
    err msg = do
      hPutStrLn stderr $ "error: " ++ msg
      exitFailure
    go f o a [] = do
      checkHelp command f
      verbList <- getVerbList o
      return (f, o, reverse a, verbList)
    go f o a ("--" : rest) =
      go f o (reverse rest ++ a) []
    go f o a (('-' : '-' : arg) : rest) =
      case HashMap.lookup arg kinds' of
        Nothing ->
          err $ "unknown argument: --" ++ arg
        Just ArgFlag
          | arg `HashSet.member` f ->
            err $ "flag cannot be used multiple times: --" ++ arg
          | otherwise ->
            go (HashSet.insert arg f) o a rest
        Just (ArgOption label)
          | arg `HashMap.member` o ->
            err $ "option cannot be used multiple times: --" ++ arg
          | otherwise ->
            let msg = "option --" ++ arg ++ " requires " ++ show label in
            case rest of
              [] -> err msg
              ('-' : '-' : _) : _ -> err msg
              parameter : rest ->
                go f (HashMap.insert arg parameter o) a rest
    go f o a (arg : rest) =
      go f o (arg : a) rest

-- | Handle the @--help@ flag
checkHelp :: String -> Flags -> IO ()
checkHelp command f
  | "help" `HashSet.member` f =
    help [command] >> exitSuccess
  | otherwise =
    return ()

-- | Handle the @--list@ option
getVerbList :: Options -> IO VerbList
getVerbList o =
  case HashMap.lookup "list" o of
    Nothing ->
      lookupEnv "TAMIL_VERB_LIST" >>= \case
        Just filePath ->
          loadVerbListFromFile filePath
        _ ->
          return defaultVerbList
    Just "-" ->
      loadVerbList getContents
    Just filePath ->
      loadVerbListFromFile filePath
  where
    loadVerbListFromFile "" =
      return defaultVerbList
    loadVerbListFromFile filePath =
      loadVerbList $ readFile filePath
    loadVerbList read = do
      (errs, verbList) <- parseAllVerbs <$> read `catch` \e -> do
        hPutStrLn stderr $ "error: cannot read verb list: " ++ show (e :: IOException)
        exitFailure
      when (not $ null errs) do
        forM_ errs $ hPutStrLn stderr
        exitFailure
      when (Set.null $ allVerbs verbList) do
        hPutStrLn stderr $ "error: verb list is empty"
        exitFailure
      return verbList

-- | Check if the input and output devices are terminals
isInteractive :: IO Bool
isInteractive = do
  stdinOpen <- hIsOpen stdin
  if stdinOpen then do
    stdinInteractive <- hIsTerminalDevice stdin
    stdoutInteractive <- hIsTerminalDevice stdout
    return $ stdinInteractive && stdoutInteractive
  else
    return False

-- | Pick the appropriate mode based on the commannd given
main :: IO ()
main =
  getArgs >>= \case
    "help" : args ->
      help args
    "learn" : args ->
      learn args
    "conjugate" : args ->
      conjugate args
    args ->
      interactive args

-- | Display help for a specific command, or the program overall
help :: [String] -> IO ()
help ("learn":_) = do
  name <- getProgName
  putStr $ unlines
    --                                                                                |
    [ "Usage: " ++ name ++ " learn [--list VERB_LIST] ... [CONFIGURATION]"
    , ""
    , "Available arguments:"
    , "  --count NUM     Set the number of questions to generate (default: 10)"
    , "  --output FILE   Save questions to a file instead of asking directly"
    , "  --strict        Only give one attempt at answering a question"
    , "  --lenient       Allow small mistakes in answers (e.g. 'r' instead of 'R')"
    , "  --definition    Only show the definition of the verb, even when ambiguous"
    , "  --verb          Always show the verb itself, even when unnecessary"
    , "  --tamil         Print verbs using only Tamil letters"
    , "  --latin         Print verbs using only Latin letters"
    , ""
    , "If a file path is provideed to output questions into, then the answers will"
    , "be stored in a file generated by appending '_key' to the filename without"
    , "the extension. If this is not desired, the option '--output_key' can be used"
    , "to specify a file path for the answer key."
    , ""
    , "Configurations can be specified by a string of the form 'KIND/TENSE/SUBJECT',"
    , "where each of the sections may be blank, and empty trailing sections may be"
    , "left off. Each section has a set of characters representing which conjugations"
    , "may be selected for a question. If the section starts with '-', then any option"
    , "except for those specified may be used. An empty section will be filled from"
    , "the corresponding section in the 'default' preset. The options are:"
    , ""
    , "KIND:             L neg. classical    a adverb        A neg. adverb"
    , "  c command       C neg. command      d conditional   D neg. conditional"
    , "  f finite        F neg. future       i infinitive    P neg. past/present"
    , "  j adjective     J neg. adjective    n noun          N neg. noun"
    , "  r relative      R neg. relative"
    , ""
    , "TENSE:            p past              r present       f future"
    , ""
    , "SUBJECT:          1 naan, naam, naangaL"
    , "                  2 nee, neengaL                      r neer"
    , "                  3 aval, avan, avar, avargaL"
    , "                  a adhu                              v avai"
    , ""
    , "Configuration presets:"
    , "  simple          fi/rf               Just present, future, and infinitive"
    , "  past            fa/p                Just past and adverb"
    , "  default         fai/-/-r            All tenses, adverb, and infinite"
    , "  extended        faijcAJCFP          Adds adjectives, commands, and negatives"
    , "  classical       faijcAJCL//-        Uses classical negatives, includes neer"
    , "  all             -//-                Includes any possible conjugation"
    , ""
    , "Presets may be used by writing their name instead of the configuration"
    , "string. The conjugations used will be selected at random from the possible"
    , "specified conjugations, with verbs selected at random from the verb list." ]
help ("conjugate":_) = do
  name <- getProgName
  putStr $ unlines
    --                                                                                |
    [ "Usage: " ++ name ++ " conjugate [--list VERB_LIST] <VERB> [ARGUMENTS...]"
    , ""
    , "Conjugation arguments:"
    , "  past            past tense"
    , "  present         present tense"
    , "  future          future tense"
    , "  infinitive      infinitive"
    , "  adverb          adverb or AvP"
    , "  adjective       verbal adjective"
    , "  relative        relative noun referring to someone who does the action"
    , "  noun            verbal noun referring to the action of the verb"
    , "  conditional/if  conditional clauses like 'if' in English"
    , "  command         imperative command"
    , "  negative        make the conjugation negative"
    , "  respectful      make the conjugation respectful (for command)"
    , "  tamil           show the result using only Tamil letters"
    , "  latin/english   show the result using only Latin letters"
    , "  guess           try to guess the conjugation for a verb"
    , "  alternative     show alternative conjugations which are valid but less common"
    , ""
    , "Arguments may be passed all together in one string or in multiple different"
    , "strings, or may be left out entirely to get an overview of the verb." ]
help _ = do
  name <- getProgName
  putStr $ unlines
    --                                                                                |
    [ "Usage: " ++ name ++ " [COMMAND] [--list VERB_LIST] ..."
    , ""
    , "If no command is given, an interactive prompt will open. To see the available"
    , "conjugations for this prompt, see '" ++ name ++ " help conjugate'. There are"
    , "also special commands available in this interactive mode. Type ':help' to view"
    , "all available commands. If no verb list is provided, a built-in verb list will"
    , "be used automatically. To override the default list, use the 'TAMIL_VERB_LIST'"
    , "environment variable. Anywhere a file path is expected, '-' can be used to"
    , "indicate that stdin/stdout/stderr should be used."
    , ""
    , "Available commands:"
    , "  conjugate       Conjugate a single verb directly from the command-line"
    , "  learn           Generate quiz questions about the conjugations of verbs"
    , "  help            View additional help information for a command"
    , ""
    , "To learn more about a command, try '" ++ name ++ " help <COMMAND>'" ]

-- | The arguments that the @learn@ command accepts
learnArgs :: ArgKinds
learnArgs = HashMap.fromList
  [ ("count", ArgOption OptionNumber)
  , ("output", ArgOption OptionFile)
  , ("output_key", ArgOption OptionFile)
  , ("strict", ArgFlag)
  , ("lenient", ArgFlag)
  , ("definition", ArgFlag)
  , ("verb", ArgFlag)
  , ("tamil", ArgFlag)
  , ("latin", ArgFlag) ]

-- | Parse the arguments for the @learn@ command and start execution
learn :: [String] -> IO ()
learn =
  parseArgs learnArgs "learn" >=> \(f, o, a, learnVerbList) -> do
    -- Replace named presets with their equivalent configuration strings
    config <-
      case a of
        [] ->
          return ""
        ["simple"] ->
          return "fi/rf"
        ["past"] ->
          return "fa/p"
        ["default"] ->
          return ""
        ["extended"] ->
          return "faijcAJCFP"
        ["classical"] ->
          return "faijcAJCL//-"
        ["all"] ->
          return "-//-"
        [config] ->
          return config
        _ -> do
          hPutStrLn stderr $ "error: 'learn' only accepts a single configuration argument"
          exitFailure
    -- Parse and validate the count arguments
    learnCount <-
      case HashMap.lookup "count" o >>= readMaybe of
        Nothing ->
          return 10
        Just count
          | count > 0, count <= 100000 ->
            return count
          | otherwise -> do
            hPutStrLn stderr $ "error: count out of range: " ++ show count ++ " (must be 1-100000)"
            exitFailure
    -- Determine how much information about a verb should be given in questions
    learnVerbStyle <-
      case ("definition" `HashSet.member` f, "verb" `HashSet.member` f) of
        (False, False) ->
          return $ VerbIfNecessary
        (True, False) ->
          return $ DefinitionOnly
        (False, True) ->
          return $ VerbAlways
        (True, True) -> do
          hPutStrLn stderr $ "error: --definition cannot be used with --verb"
          exitFailure
    -- Determine which file handles the questions and answers will be written to, if any
    learnOutput <-
      let
        treatEmptyAsMissing "" = Nothing
        treatEmptyAsMissing path = Just path
        open path =
          openFile path WriteMode `catch` \e -> do
            hPutStrLn stderr $ "error: cannot write output to file: " ++ show (e :: IOException)
            exitFailure
        openOrStd "-" = return stdout
        openOrStd path = open path
        addSuffix path suffix =
          let (filename, ext) = splitExtension path in
          addExtension (filename ++ "_" ++ suffix) ext
      in
        case ( HashMap.lookup "output"     o >>= treatEmptyAsMissing
             , HashMap.lookup "output_key" o >>= treatEmptyAsMissing ) of
          -- If no output file is given, interactive mode is used
          (Nothing, Nothing) ->
            isInteractive >>= \case
              False -> return $ Just (stdout, stderr)
              True  -> return Nothing
          -- If one output file is stdout and the other is unspecified, use stderr for the other
          (Just "-", Nothing) ->
            return $ Just (stdout, stderr)
          (Nothing, Just "-") ->
            return $ Just (stderr, stdout)
          -- If just --output is specified, derive a filename for the answer key
          (Just path, Nothing) -> do
            hq <- open path
            ha <- open $ addSuffix path "key"
            return $ Just (hq, ha)
          -- If just --output_key is specified, derive a filename for the generated questions
          (Nothing, Just path) -> do
            ha <- open path
            hq <- open $ addSuffix path "questions"
            return $ Just (hq, ha)
          -- If both files are specified, just use those files (or stdout for '-')
          (Just q, Just a) -> do
            hq <- openOrStd q
            ha <- openOrStd a
            return $ Just (hq, ha)
    let
      -- Determine if there are second chances for close answers or not
      learnStrict = "strict" `HashSet.member` f
      -- Determine how to compare answers for correctness
      learnCheck
        | "lenient" `HashSet.member` f = pLenient
        | otherwise = (==)
      -- Determine how to display Tamil strings
      learnShowTamil =
        case ("tamil" `HashSet.member` f, "latin" `HashSet.member` f) of
          (True, False) ->
            toTamil
          (False, True) ->
            toLatin
          _ ->
            show
    startLearn LearnSettings { .. } config

-- | Parse the arguments for the @conjugate@ command and start execution
conjugate :: [String] -> IO ()
conjugate =
  parseArgs HashMap.empty "conjugate" >=> \(_, _, a, verbList) -> do
    case a of
      [] -> do
        hPutStrLn stderr "error: no verb specified"
        exitFailure
      verb : args ->
        processRequest verbList verb $ unwords args

-- | Start interactive mode by handling command-line arguments and calling 'startInteractive'
interactive :: [String] -> IO ()
interactive =
  parseArgs HashMap.empty "interactive" >=> \(_, _, a, verbList) ->
    case a of
      [] ->
        isInteractive >>= \case
          False -> do
            hPutStrLn stderr $ "error: no command was given, but interactive mode cannot be used"
            exitFailure
          True -> do
            putStrLn "Enter a word and a way to conjugate it."
            startInteractive verbList
      command : _ -> do
        name <- getProgName
        hPutStrLn stderr $ "error: unknown command: " ++ command ++ " (see '" ++ name ++ " help' for more info)"
        exitFailure

