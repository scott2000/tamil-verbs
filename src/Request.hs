module Request where

import TamilString
import Verb
import Conjugation

import Control.Monad

import Data.List
import Data.Char

import System.IO

import qualified Data.HashMap.Strict as HashMap

data TypeRequest
  = TRAdjective
  | TRNoun
  | TRAdverb
  | TRInfinitive
  | TRCommand
  deriving Eq

instance Show TypeRequest where
  show = \case
    TRAdjective  -> "adjective"
    TRNoun       -> "noun"
    TRAdverb     -> "adverb"
    TRInfinitive -> "infinitive"
    TRCommand    -> "command"

data TenseRequest
  = TRPast
  | TRPresent
  | TRFuture
  | TRHabitual
  deriving Eq

instance Show TenseRequest where
  show = \case
    TRPast     -> "past"
    TRPresent  -> "present"
    TRFuture   -> "future"
    TRHabitual -> "habitual"

data ConjugationRequest = ConjugationRequest
  { crNegative :: Bool
  , crRespectful :: Bool
  , crType :: Maybe TypeRequest
  , crTense :: Maybe TenseRequest
  , crSubject :: Maybe Subject
  , crGuess :: Bool
  , crError :: Bool }

getConjugations :: Verb -> ConjugationRequest -> [Conjugation]
getConjugations verb cr =
  if crError cr then [] else
  if crNegative cr then
    if crRespectful cr then
      case crType cr of
        Just TRCommand ->
          [Negative $ NegativeCommand True]
        _ ->
          parseNegative
    else
      parseNegative
  else
    if crRespectful cr then
      case crType cr of
        Just TRCommand ->
          [Positive $ Command True]
        _ ->
          parsePositive
    else
      parsePositive
  where
    defective = verbDefective verb
    subject =
      case crSubject cr of
        Just subject ->
          subject
        Nothing
          | defective -> Irrational Adhu
          | otherwise -> Naan
    parseNegative =
      case crType cr of
        Just TRAdjective ->
          [Negative NegativeAdjective]
        Just TRNoun ->
          [Negative NegativeNoun]
        Just TRAdverb ->
          [Negative NegativeAdverb]
        Just TRInfinitive ->
          [Infinitive]
        Just TRCommand ->
          [Negative $ NegativeCommand False]
        Nothing ->
          case crTense cr of
            Just TRFuture ->
              [Negative $ NegativeFuture subject]
            Just TRHabitual ->
              [Negative NegativeHabitual]
            _ ->
              [Negative NegativePastPresent]
    parsePositive =
      case crType cr of
        Just TRAdjective ->
          map (Positive . Adjective) tenses
        Just TRNoun ->
          [Positive Noun]
        Just TRAdverb ->
          [Positive Adverb]
        Just TRInfinitive -> do
          [Infinitive]
        Just TRCommand ->
          [Positive $ Command False]
        Nothing ->
          map (\tense -> Positive $ Finite tense subject) tenses
      where
        tenses =
          case crTense cr of
            Just TRPast -> [Past]
            Just TRPresent -> [Present]
            Just TRFuture -> [Future]
            Just TRHabitual -> [Future]
            Nothing
              | defective -> [Future]
              | otherwise -> [Past, Present, Future]

parseConjugationRequest :: [String] -> IO ConjugationRequest
parseConjugationRequest parts = do
  let
    defaultRequest = ConjugationRequest
      { crNegative = False
      , crRespectful = False
      , crType = Nothing
      , crTense = Nothing
      , crSubject = Nothing
      , crGuess = False
      , crError = False }
  request <- foldM go defaultRequest parts
  case request of
    ConjugationRequest { crNegative = True, crType = Just TRInfinitive } -> do
      hPutStrLn stderr "error: infinitives cannot be negative"
      return request { crError = True }
    ConjugationRequest { crRespectful = True, crType = Nothing, crTense = Nothing, crSubject = Nothing } ->
      return request { crType = Just TRCommand }
    ConjugationRequest { crRespectful = True, crType = Just TRCommand } ->
      return request
    ConjugationRequest { crRespectful = True } -> do
      hPutStrLn stderr "warning: only commands can be made respectful"
      return request
    _ ->
      return request
  where
    go cr s =
      case map toLower s of
        "negative" ->
          return cr { crNegative = True }
        "respectful" ->
          return cr { crRespectful = True }
        "guess" ->
          return cr { crGuess = True }
        "adjective" ->
          updateType TRAdjective
        "noun" ->
          updateType TRNoun
        "adverb" ->
          updateType TRAdverb
        "infinitive" ->
          updateType TRInfinitive
        "command" ->
          updateType TRCommand
        "past" ->
          updateTense TRPast
        "present" ->
          updateTense TRPresent
        "future" ->
          updateTense TRFuture
        "habitual" ->
          updateTense TRHabitual
        "neg" ->
          return cr { crNegative = True }
        "res" ->
          return cr { crRespectful = True }
        "adj" ->
          updateType TRAdjective
        "inf" ->
          updateType TRInfinitive
        "com" ->
          updateType TRCommand
        "pres" ->
          updateTense TRPresent
        "fut" ->
          updateTense TRFuture
        "hab" ->
          updateTense TRHabitual
        other ->
          case
            case parseTamil other of
              Left _ -> Nothing
              Right subject ->
                let
                  normalized = normalize subject
                  findSubject [] = Nothing
                  findSubject (s:ss)
                    | normalize (tamilShow s) == normalized = Just s
                    | otherwise                             = findSubject ss
                in
                  findSubject allSubjects
          of
            Just subject ->
              updateSubject subject
            Nothing -> do
              hPutStrLn stderr $ "error: unknown conjugation argument: " ++ other
              return cr { crError = True }
      where
        updateType = update crType \crType -> cr { crType }
        updateTense = update crTense \crTense -> cr { crTense }
        updateSubject = update crSubject \crSubject -> cr { crSubject }
        update get put new = do
          case get cr of
            Just old | old /= new -> do
              hPutStrLn stderr $ "warning: " ++ show new ++ " overrides " ++ show old
            _ ->
              return ()
          return $ put $ Just new

guess :: VerbList -> TamilString -> [(String, Verb)]
guess verbList basicRoot =
  checkFor irregularVerbs $ checkFor verbList $
    case reducedStr of
      -- These are very hard to guess due to adding euphonic U
      Vowel (U Short) : Consonant (Medium LAlveolar)  : _ -> []
      Vowel (U Short) : Consonant (Medium R)          : _ -> []
      Vowel (U Short) : Consonant (Medium LRetroflex) : _ -> []
      Vowel (U Short) : Consonant (Soft NRetroflex)   : _ -> []
      Vowel (U Short) : Consonant (Soft NAlveolar)    : _ -> []
      Consonant (Medium Y) : _ ->
        [basicClass $ Class1 Weak]
      _
        | isShort ->
          case reducedStr of
            Vowel (U Short) : Consonant (Hard K) : Vowel _ : _ ->
              [basicClass $ Class1 Weak]
            Vowel (U Short) : Consonant (Hard TRetroflex) : Vowel _ : _ ->
              [basicClass $ Class1 Weak]
            Vowel (U Short) : Consonant (Hard RAlveolar) : Vowel _ : _ ->
              [basicClass $ Class1 Weak]
            Consonant _ : _ ->
              [basicClass $ Class2 Weak]
            Vowel (A Short) : _ ->
              [basicClass $ Class2 Strong]
            Vowel (I Short) : Consonant (Medium R) : _ ->
              [defective2W]
            _ ->
              [basicClass $ Class1 Strong, defective2W]
        | otherwise ->
          case reducedStr of
            Vowel (U Short) : Consonant (Hard TRetroflex) : Vowel (I Short) : _ ->
              [basicClass $ Class1 Weak]
            Vowel (U Short) : _ ->
              [basicClass Class3]
            Vowel v : _ | not $ isShortishVowel v ->
              [basicClass Class3]
            _ ->
              [basicClass $ Class2 Weak]
  where
    reducedRoot = TamilString reducedStr
    reducedStr =
      case untamil basicRoot of
        Vowel (U Short) : Consonant (Hard K) : rest@(Vowel v : _) | not $ isShortVowel v -> rest
        str -> str
    checkFor verbList other =
      case go $ sortOn (\(TamilString root, _) -> -(length root)) $ HashMap.toList $ byRoot verbList of
        [] -> other
        verbs -> verbs
    go [] = []
    go ((root, verbs):rest) =
      case stripSuffix root reducedRoot of
        Nothing -> go rest
        Just rootPrefix ->
          let
            updateVerb v =
              ( show (verbClass v)
              , v { verbPrefix = forChoice (verbPrefix v) \root -> rootPrefix `append` root } )
          in
            map updateVerb verbs
    basicClass c =
      ( show c
      , defaultVerb
        { verbRoot = reducedRoot
        , verbClass = c } )
    defective2W =
      ( show $ Class2 Weak
      , defaultVerb
          { verbRoot = reducedRoot
          , verbDefective = True
          , verbClass = Class2 Weak } )
    isShortishVowel = \case
      Ai -> True
      Au -> True
      v  -> isShortVowel v
    isShort =
      case reducedStr of
        [Consonant _, Vowel v] ->
          isShortishVowel v
        [Consonant _, Vowel v, Consonant _] ->
          isShortishVowel v
        [Vowel v0, Consonant _, Vowel v1] ->
          isShortishVowel v0 && isShortishVowel v1
        [Vowel v0, Consonant _, Vowel v1, Consonant _] ->
          isShortishVowel v0 && isShortishVowel v1
        _ ->
          False

lookupVerb :: VerbList -> Bool -> String -> Either String [(String, Verb)]
lookupVerb verbList allowGuess word =
  case HashMap.lookup word $ byDefinition verbList of
    Just verbs ->
      Right $ map (\verb -> (show $ verbRoot verb, verb)) $ sort verbs
    Nothing ->
      case parseTamil word of
        Left err ->
          if any isUpper word then
            Left $ show err
          else if any isSpace word then
            Left "cannot find word with that definition"
          else
            Left $ "not found as definition and not valid as Tamil (" ++ show err ++ ")"
        Right tamil ->
          case HashMap.lookup tamil $ byRoot verbList of
            Just verbs ->
              Right $ map (\verb -> (intercalate ", " $ verbDefinitions verb, verb)) $ sort verbs
            Nothing ->
              if allowGuess then
                case guess verbList tamil of
                  [] ->
                    Left "couldn't make a good guess (make sure there isn't an extra U)"
                  verbs ->
                    Right verbs
              else
                Left $ "verb not found" ++
                  let
                    normalized = normalize tamil
                    suggestions = filter ((normalized ==) . normalize) $ HashMap.keys $ byRoot verbList
                  in
                    case take 3 $ sort suggestions of
                      [] ->
                        case guess verbList tamil of
                          [] -> ""
                          _ ->
                            -- Only recommend if it'll actually work
                            " (maybe try again with 'guess'?)"
                      suggestions ->
                        let showFunction = if all isAscii word then toLatin else toTamil in
                        " (did you mean " ++ intercalate " or " (map showFunction suggestions) ++ "?)"

processRequest :: String -> String -> IO ()
processRequest verb conjugation = do
  let word = stripTo $ unwords $ splitHyphen verb
  request <- parseConjugationRequest $ splitHyphen conjugation
  when (not $ crError request)
    case lookupVerb defaultVerbList (crGuess request) word of
      Left err ->
        hPutStrLn stderr $ "error: " ++ err
      Right [(_, verb)] ->
        forM_ (getConjugations verb request) \conjugation ->
          putStrLn $ show $ conjugate conjugation verb
      Right verbs ->
        forM_ verbs \(header, verb) ->
          case getConjugations verb request of
            [] -> return ()
            [conjugation] ->
              putStrLn $ header ++ ": " ++ show (conjugate conjugation verb)
            conjugations -> do
              putStrLn $ header ++ ":"
              forM_ conjugations \conjugation ->
                putStrLn $ "  " ++ show (conjugate conjugation verb)

splitHyphen :: String -> [String]
splitHyphen s =
  case dropWhile isBreak s of
    "" -> []
    s' ->
      let (w, s'') = break isBreak s' in
      w : splitHyphen s''
  where
    isBreak = \case
      '-' -> True
      ',' -> True
      ' ' -> True
      _   -> False

irregularVerbs :: VerbList
irregularVerbs = foldl' (flip addVerb) emptyVerbList
  [ defaultVerb
      { verbRoot = "pODu"
      , verbClass = Class1 Weak }
  , defaultVerb
      { verbRoot = "saa"
      , verbPast = Just "sett"
      , verbClass = Class1 Weak }
  , defaultVerb
      { verbRoot = "azhu"
      , verbClass = Class1 Weak }
  , defaultVerb
      { verbRoot = "kEL"
      , verbClass = Class1 Strong }
  , defaultVerb
      { verbRoot = "El"
      , verbClass = Class1 Strong }
  , defaultVerb
      { verbRoot = "kal"
      , verbClass = Class1 Strong }
  , defaultVerb
      { verbRoot = "vil"
      , verbClass = Class1 Strong }
  , defaultVerb
      { verbRoot = "thOl"
      , verbClass = Class1 Strong }
  , defaultVerb
      { verbRoot = "eDu"
      , verbClass = Class1 Strong }
  , defaultVerb
      { verbRoot = "paar"
      , verbClass = Class1 Strong }
  , defaultVerb
      { verbRoot = "thaakkuRu"
      , verbPast = Just "thaakkuND"
      , verbClass = Class2 Weak }
  , defaultVerb
      { verbRoot = "thaa"
      , verbRespectfulCommand = Just "thaarungaL"
      , verbPast = Just "thandh"
      , verbStem = Just "tharu"
      , verbClass = Class2 Weak }
  , defaultVerb
      { verbRoot = "vaa"
      , verbRespectfulCommand = Just "vaarungaL"
      , verbPast = Just "vandh"
      , verbStem = Just "varu"
      , verbClass = Class2 Weak }
  , defaultVerb
      { verbRoot = "pOdhu"
      , verbDefective = True
      , verbPast = Just $ ChoiceString ["pOndh"] ["pOrndh"] []
      , verbClass = Class2 Weak }
  , defaultVerb
      { verbRoot = "kaaN"
      , verbPast = Just "kaND"
      , verbClass = Class2 Weak }
  , defaultVerb
      { verbRoot = "nil"
      , verbClass = Class2 Strong }
  , defaultVerb
      { verbRoot = "iru"
      , verbClass = Class2 Strong }
  , defaultVerb
      { verbRoot = "aa"
      , verbClass = Class3 }
  , defaultVerb
      { verbRoot = "pO"
      , verbClass = Class3 }
  , defaultVerb
      { verbRoot = "sol"
      , verbClass = Class3 } ]

defaultVerbList :: VerbList
defaultVerbList = foldl' (flip addVerb) emptyVerbList
  [ -- Class 1 Weak
    defaultVerb
      { verbRoot = "saappiDu"
      , verbDefinitions = ["eat"]
      , verbClass = Class1 Weak }
  , defaultVerb
      { verbRoot = "pODu"
      , verbDefinitions = ["drop", "place"]
      , verbClass = Class1 Weak }
  , defaultVerb
      { verbRoot = "viDu"
      , verbDefinitions = ["let go", "release"]
      , verbClass = Class1 Weak }
  , defaultVerb
      { verbRoot = "sey"
      , verbDefinitions = ["do", "make"]
      , verbClass = Class1 Weak }
    -- Class 1 Strong
  , defaultVerb
      { verbRoot = "kEL"
      , verbDefinitions = ["ask", "listen", "hear"]
      , verbClass = Class1 Strong }
  , defaultVerb
      { verbRoot = "eDu"
      , verbDefinitions = ["take", "get"]
      , verbClass = Class1 Strong }
  , defaultVerb
      { verbRoot = "kuDi"
      , verbDefinitions = ["drink"]
      , verbClass = Class1 Strong }
  , defaultVerb
      { verbRoot = "koDu"
      , verbDefinitions = ["give"]
      , verbClass = Class1 Strong }
  , defaultVerb
      { verbRoot = "paDi"
      , verbDefinitions = ["study", "read"]
      , verbClass = Class1 Strong }
  , defaultVerb
      { verbRoot = "paDu"
      , verbDefinitions = ["lie down"]
      , verbClass = Class1 Strong }
  , defaultVerb
      { verbRoot = "paar"
      , verbDefinitions = ["look", "see"]
      , verbClass = Class1 Strong }
  , defaultVerb
      { verbRoot = "muDi"
      , verbDefinitions = ["finish"]
      , verbClass = Class1 Strong }
  , defaultVerb
      { verbRoot = "uDai"
      , verbDefinitions = ["break"]
      , verbClass = Class1 Strong }
    -- Class 2 Weak
  , defaultVerb
      { verbRoot = "thaa"
      , verbDefinitions = ["give"]
      , verbRespectfulCommand = Just "thaarungaL"
      , verbPast = Just "thandh"
      , verbStem = Just "tharu"
      , verbClass = Class2 Weak }
  , defaultVerb
      { verbRoot = "vaa"
      , verbDefinitions = ["come"]
      , verbRespectfulCommand = Just "vaarungaL"
      , verbPast = Just "vandh"
      , verbStem = Just "varu"
      , verbClass = Class2 Weak }
  , defaultVerb
      { verbRoot = "koL"
      , verbDefinitions = ["have"]
      , verbClass = Class2 Weak }
  , defaultVerb
      { verbRoot = "kol"
      , verbDefinitions = ["kill"]
      , verbClass = Class2 Weak }
  , defaultVerb
      { verbRoot = "chel"
      , verbDefinitions = ["go"]
      , verbClass = Class2 Weak }
  , defaultVerb
      { verbRoot = "muDi"
      , verbDefinitions = ["be finished"]
      , verbDefective = True
      , verbClass = Class2 Weak }
  , defaultVerb
      { verbRoot = "uDai"
      , verbDefinitions = ["be broken"]
      , verbDefective = True
      , verbClass = Class2 Weak }
  , defaultVerb
      { verbRoot = "teri"
      , verbDefinitions = ["be known"]
      , verbDefective = True
      , verbClass = Class2 Weak }
  , defaultVerb
      { verbRoot = "puri"
      , verbDefinitions = ["be understood"]
      , verbDefective = True
      , verbClass = Class2 Weak }
  , defaultVerb
      { verbRoot = "uTkaar"
      , verbDefinitions = ["sit"]
      , verbClass = Class2 Weak }
    -- Class 2 Strong
  , defaultVerb
      { verbRoot = "nil"
      , verbDefinitions = ["stop", "stand"]
      , verbClass = Class2 Strong }
  , defaultVerb
      { verbRoot = "naDa"
      , verbDefinitions = ["walk", "happen"]
      , verbClass = Class2 Strong }
  , defaultVerb
      { verbRoot = "paRa"
      , verbDefinitions = ["fly"]
      , verbClass = Class2 Strong }
  , defaultVerb
      { verbRoot = "maRa"
      , verbDefinitions = ["forget"]
      , verbClass = Class2 Strong }
  , defaultVerb
      { verbRoot = "iru"
      , verbDefinitions = ["be", "stay"]
      , verbClass = Class2 Strong }
    -- Class 3
  , defaultVerb
      { verbRoot = "pO"
      , verbDefinitions = ["go", "leave"]
      , verbClass = Class3 }
  , defaultVerb
      { verbRoot = "sol"
      , verbDefinitions = ["say", "tell"]
      , verbClass = Class3 }
  , defaultVerb
      { verbRoot = "ODu"
      , verbDefinitions = ["run"]
      , verbClass = Class3 }
  , defaultVerb
      { verbRoot = "thoongu"
      , verbDefinitions = ["sleep"]
      , verbClass = Class3 }
  , defaultVerb
      { verbRoot = "tirumbu"
      , verbDefinitions = ["turn around", "return"]
      , verbClass = Class3 }
  , defaultVerb
      { verbRoot = "paaDu"
      , verbDefinitions = ["sing"]
      , verbClass = Class3 }
  , defaultVerb
      { verbRoot = "pEsu"
      , verbDefinitions = ["speak", "talk"]
      , verbClass = Class3 }
  , defaultVerb
      { verbRoot = "veTTu"
      , verbDefinitions = ["cut"]
      , verbClass = Class3 }
  , defaultVerb
      { verbRoot = "vaangu"
      , verbDefinitions = ["buy"]
      , verbClass = Class3 }
  ]

