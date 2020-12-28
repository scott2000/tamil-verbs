-- | Generate quiz questions about verb conjugations
module Learn where

import TamilString
import Verb
import Conjugation
import Interactive

import System.IO
import System.Exit
import System.Random.MWC
import System.Random.Stateful hiding (split)

import Data.List
import Data.Monoid

import qualified Data.Set as Set
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.HashMap.Strict as HashMap

-- | Represents a weighted set of possible configuration options
data WeightedConfigPool = WeightedConfigPool
  { -- | A map from the start index of an interval to the option filling that interval
    wcpOptions :: !(IntMap Char)
    -- | The sum of weights which will represent the end of the last interval
  , wcpSum :: !Int }

instance Show WeightedConfigPool where
  show WeightedConfigPool { .. } =
    snd $ go $ IntMap.toList wcpOptions
    where
      go [] = (wcpSum, "")
      go ((a, ch) : rest) =
        let (b, str) = go rest in
        (a, replicate (b - a) ch ++ str)

-- | An empty 'WeightedConfigPool'
wcpEmpty :: WeightedConfigPool
wcpEmpty = WeightedConfigPool
  { wcpOptions = IntMap.empty
  , wcpSum = 0 }

-- | Insert an option with a given weight into the 'WeightedConfigPool'
wcpInsert :: Char -> Int -> WeightedConfigPool -> WeightedConfigPool
wcpInsert ch w WeightedConfigPool { .. } = WeightedConfigPool
  { wcpOptions = IntMap.insert wcpSum ch wcpOptions
  , wcpSum = wcpSum + w }

-- | Create a 'WeightedConfigPool' from 'Weights'
wcpFromWeights :: Weights -> WeightedConfigPool
wcpFromWeights = foldl' (flip $ uncurry wcpInsert) wcpEmpty . Map.toList

-- | Draw an option from a 'WeightedConfigPool'
wcpDraw :: WeightedConfigPool -> GenIO -> IO Char
wcpDraw p g = do
  k <- upTo g $ wcpSum p
  case IntMap.lookupLE k $ wcpOptions p of
    Just (_, ch) ->
      return ch
    Nothing ->
      fail "wcpDraw: out of bounds"

-- | Generate an integer in the range @[0, upperBound)@
upTo :: GenIO -> Int -> IO Int
upTo g upperBound
  | upperBound <= 0 = fail $ "upTo: invalid upper bound: " ++ show upperBound
  | otherwise =
    fromIntegral <$> uniformWord32R (fromIntegral $ upperBound - 1) g

-- | Select an element from a list at random
oneOf :: GenIO -> [a] -> IO a
oneOf g xs = do
  k <- upTo g $ length xs
  return $ xs !! fromIntegral k

-- | Generate a random 'Bool'
randomBool :: GenIO -> IO Bool
randomBool g = (0 /=) <$> uniformWord32R 2 g

-- | A configuration specifying the weighted probabilities of each option being chosen
data Config = Config
  { -- | The options for @KIND@
    cKind :: !WeightedConfigPool
    -- | The options for @TENSE@
  , cTense :: !WeightedConfigPool
    -- | The options for @SUBJECT@
  , cSubject :: !WeightedConfigPool
    -- | The options for @KIND@ when a verb is defective
  , cKindDefective :: !WeightedConfigPool
    -- | The options for @SUBJECT@ when the conjugation is relative
  , cSubject3P :: !WeightedConfigPool }

-- | Pick a random conjugation from 'Config' for a given 'Verb' with a definition
randomConjugation :: Config -> Verb -> VerbDefinition -> GenIO -> IO Conjugation
randomConjugation Config { .. } verb definition g =
  wcpDraw kind g >>= \case
    'L' -> Negative . NegativeClassical <$> subject
    'a' -> return $ Positive Adverb
    'A' -> return $ Negative NegativeAdverb
    'c' -> return $ Positive $ Command True
    'C' -> Negative . NegativeCommand <$> randomBool g
    'd' -> return $ Positive Conditional
    'D' -> return $ Negative NegativeConditional
    'f' -> Positive <$> (Finite <$> tense <*> subject)
    'F' -> Negative . NegativeFuture <$> subject
    'i' -> return Infinitive
    'P' -> return $ Negative NegativePastPresent
    'j' -> Positive . Adjective <$> tense
    'J' -> return $ Negative NegativeAdjective
    'n' -> return $ Positive Noun
    'N' -> return $ Negative NegativeNoun
    'r' -> Positive <$> (Relative <$> tense <*> subject3P)
    'R' -> Negative . NegativeRelative <$> subject3P
    _ -> fail "invalid kind"
  where
    defective = verbDefective verb
    inanimate
      | verbInanimate verb = True
      | 'b' : 'e' : ' ' : _ <- vDefinition definition = True
      | otherwise = False
    kind
      | defective = cKindDefective
      | otherwise = cKind
    tense
      | defective = return Future
      | otherwise =
        wcpDraw cTense g >>= \case
          'p' -> return Past
          'r' -> return Present
          'f' -> return Future
          _ -> fail "invalid tense"
    subject
      | inanimate || defective =
        return $ Third $ Irrational Adhu
      | otherwise =
        wcpDraw cSubject g >>= \case
          '1' -> oneOf g [Naan, Naan, Naan, Naam, Naam, Naangal]
          '2' -> oneOf g [Nee, Nee, Neengal]
          'r' -> return Neer
          '3' -> Third <$> oneOf g [Aval, Avan, Avar, Avar, Avargal]
          'a' -> return $ Third $ Irrational Adhu
          'v' -> return $ Third $ Irrational Avai
          _ -> fail "invalid subject"
    subject3P
      | defective =
        return $ Irrational Adhu
      | otherwise =
        wcpDraw cSubject3P g >>= \case
          '3' -> oneOf g [Aval, Avan, Avar, Avar, Avargal, Avargal]
          'a' -> return $ Irrational Adhu
          'v' -> return $ Irrational Avai
          _ -> fail "invalid third-person subject"

-- | Maps options to their weight
type Weights = Map Char Int

-- | Assigns the same weight to all options in a string
weights :: String -> Int -> [(Char, Int)]
weights str w = map (\ch -> (ch, w)) str

-- | The weights for 'cKind'
kindWeights :: Weights
kindWeights = Map.fromList $ concat
  [ weights "cCDJNR" 1
  , weights "djnrFPA" 2
  , weights "aiL" 4
  , weights "f" 16 ]

-- | The weights for 'cTense'
tenseWeights :: Weights
tenseWeights = Map.fromList
  [ ('p', 5)
  , ('r', 2)
  , ('f', 3) ]

-- | The weights for 'cSubject'
subjectWeights :: Weights
subjectWeights = Map.fromList $ concat
  [ weights "r" 1
  , weights "2v" 2
  , weights "13a" 3 ]

-- | The weights for 'cKindDefective'
kindDefectiveWeights :: Weights
kindDefectiveWeights =
  Map.withoutKeys kindWeights $ Set.fromList "acCdD"

-- | The weights for 'cSubject3P'
subject3PWeights :: Weights
subject3PWeights = Map.fromList
  [ ('v', 1)
  , ('a', 2)
  , ('3', 3) ]

-- | Split a configuration string of the form @KIND/TENSE/SUBJECT@ into its component parts
splitSections :: String -> Either String (String, String, String)
splitSections str =
  let
    (kind, str') = breakSlash str
    (tense, str'') = breakSlash $ dropSlash str'
    (subject, str''') = breakSlash $ dropSlash str''
  in
    if null str''' then
      Right (kind, tense, subject)
    else
      Left "too many sections in configuration string"
  where
    breakSlash = break ('/' ==)
    dropSlash = \case
      '/' : rest -> rest
      other -> other

-- | Parse a section of a configuration string based on the weights and defaults for that section
parseSection :: String -> Weights -> String -> String -> Either String WeightedConfigPool
parseSection section ws def str =
  case str of
    '-' : str ->
      goNeg ws str
    _ ->
      go wcpEmpty Set.empty str
  where
    repeatedChar ch =
      Left $ "character '" ++ [ch] ++ "' should only be listed once in " ++ section ++ " section"
    invalidChar ch =
      Left $ "character '" ++ [ch] ++ "' is invalid for " ++ section ++ " section"
    wcpDefault =
      wcpFromWeights $ Map.restrictKeys ws $ Set.fromList def
    -- Parse a negative section
    goNeg m []
      | Map.null m =
        Right wcpDefault
      | otherwise =
        Right $ wcpFromWeights m
    goNeg m (ch : rest)
      | ch `Map.member` m =
        goNeg (Map.delete ch m) rest
      | ch `Map.member` ws =
        repeatedChar ch
      | otherwise =
        invalidChar ch
    -- Parse a regular section that isn't negative
    go p s []
      | Set.null s =
        Right wcpDefault
      | otherwise =
        Right p
    go p s (ch : rest)
      | ch `Set.member` s =
        repeatedChar ch
      | Just w <- Map.lookup ch ws =
        go (wcpInsert ch w p) (Set.insert ch s) rest
      | otherwise =
        invalidChar ch

-- | Like 'parseSection', but doesn't check if the input is valid
restrictSection :: Weights -> String -> WeightedConfigPool
restrictSection ws str
  | Map.null restrictedWeights =
    wcpFromWeights ws
  | otherwise =
    wcpFromWeights restrictedWeights
  where
    restrictedWeights =
      case str of
        '-' : rest ->
          Map.withoutKeys ws $ Set.fromList rest
        _ ->
          Map.restrictKeys ws $ Set.fromList str

-- | Parses a configuration string of the form @KIND/TENSE/SUBJECT@ to create a 'Config'
parseConfig :: String -> Either String Config
parseConfig str = do
  (kind, tense, subject) <- splitSections str
  cKind <- parseSection "KIND" kindWeights "fai" kind
  cTense <- parseSection "TENSE" tenseWeights "prf" tense
  cSubject <- parseSection "SUBJECT" subjectWeights "123av" subject
  let
    cKindDefective = restrictSection kindDefectiveWeights kind
    cSubject3P = restrictSection subject3PWeights subject
  return Config { .. }

-- | Formats a conjugation for a verb to be used in a question
showConjugation :: (TamilString -> String) -> String -> Conjugation -> String
showConjugation showTamil word = \case
  Positive positive ->
    case positive of
      Finite tense subject ->
        sTense tense ++ " of " ++ word ++ " for " ++ sSub subject
      Adjective tense ->
        sTense tense ++ " adjective of " ++ word
      Relative tense subject ->
        sTense tense ++ " relative pronoun of " ++ word ++ " for " ++ sSub subject
      Noun ->
        "verbal noun of " ++ word
      Adverb ->
        "adverb of " ++ word
      Conditional ->
        "conditional of " ++ word
      Command False ->
        "command of " ++ word
      Command True ->
        "respectful command of " ++ word
  Negative negative ->
    case negative of
      NegativePastPresent ->
        "negative past/present of " ++ word
      NegativeFuture subject ->
        "negative future of " ++ word ++ " for " ++ sSub subject
      NegativeClassical subject ->
        "classical negative of " ++ word ++ " for " ++ sSub subject
      NegativeAdjective ->
        "negative adjective of " ++ word
      NegativeRelative subject ->
        "negative relative pronoun of " ++ word ++ " for " ++ sSub subject
      NegativeNoun ->
        "negative verbal noun of " ++ word
      NegativeAdverb ->
        "negative adverb of " ++ word
      NegativeConditional ->
        "negative conditional of " ++ word
      NegativeCommand False ->
        "negative command of " ++ word
      NegativeCommand True ->
        "negative respectful command of " ++ word
  Infinitive ->
    "infinitive of " ++ word
  where
    sSub :: TamilShow a => a -> String
    sSub = showTamil . tamilShow
    sTense tense =
      show tense ++ " tense"

-- | Represents the possible arguments that the @learn@ command can accept
data LearnSettings = LearnSettings
  { learnVerbList :: !VerbList
  , learnCount :: !Int
  , learnOutput :: !(Maybe (Handle, Handle))
  , learnStrict :: !Bool
  , learnCheck :: !(TamilLetter -> TamilLetter -> Bool)
  , learnVerbStyle :: !VerbStyle
  , learnShowTamil :: !(TamilString -> String) }

-- | Determines in what situations the verb will be given in the question
data VerbStyle
  = DefinitionOnly  -- ^ Never give the verb, only the definition
  | VerbIfNecessary -- ^ Give the verb if it would make the question less ambiguous
  | VerbAlways      -- ^ Always give the verb even if it isn't ambiguous

-- | Pick a definition at random, but with decreasing probability for alternative definitions
pickDefinition :: GenIO -> [VerbDefinition] -> IO (Maybe VerbDefinition)
pickDefinition _ [] =
  return Nothing
pickDefinition _ [def] =
  return $ Just def
pickDefinition g (def : rest) =
  randomBool g >>= \case
    False ->
      pickDefinition g rest
    True ->
      return $ Just def

-- | Find all verbs that match a definition exactly (including the note)
matchingDefinition :: VerbList -> VerbDefinition -> String -> [Verb]
matchingDefinition verbList definition note =
  filter hasExactMatch $ Set.toList
    case HashMap.lookup (vDefinition definition) $ byDefinition verbList of
      Nothing -> error "matchingDefinition: invalid definition"
      Just verbs -> verbs
  where
    hasExactMatch verb =
      verbNote verb == note && definition `elem` verbDefinitions verb

-- | Generate questions to ask based on the settings, and pass them to a provided function
generateQuestions :: Monoid a => LearnSettings -> Config -> (Int -> String -> [ChoiceString] -> IO a) -> IO a
generateQuestions LearnSettings { .. } config f = do
  g <- createSystemRandom
  go g 1 mempty Set.empty
  where
    go g n correct verbs
      | n > learnCount = return correct
      | Set.null verbs =
        go g n correct $ allVerbs learnVerbList
      | otherwise = do
        -- Pick a random verb
        k <- upTo g $ Set.size verbs
        let
          verb = Set.elemAt k verbs
          verbs' = Set.deleteAt k verbs
        -- Pick a definition for the question
        pickDefinition g (verbDefinitions verb) >>= \case
          Nothing ->
            -- If there is no definition, skip the verb
            go g n correct verbs'
          Just definition -> do
            -- Pick a random conjugation that fits the verb
            conjugation <- randomConjugation config verb definition g
            let
              -- Find verbs that match the definition
              matchingVerbs = matchingDefinition learnVerbList definition $ verbNote verb
              -- Find verbs that match both definition and root
              fullVerb = getFullVerb verb
              filteredMatching = flip filter matchingVerbs \verb ->
                getFullVerb verb == fullVerb
              -- Include the verb if necessary if VerbIfNecessary is set, otherwise follow the specified flag
              includeVerb =
                case learnVerbStyle of
                  DefinitionOnly -> False
                  VerbIfNecessary ->
                    case matchingVerbs of
                      [_] -> False
                      _ ->
                        length filteredMatching < length matchingVerbs
                  VerbAlways -> True
              -- Format the definition so that the main definition is in quotes but not the note
              definitionString =
                "\"" ++ vDefinition definition ++ "\"" ++
                  case vDefinitionNote definition of
                    "" -> ""
                    note -> " (" ++ note ++ ")"
              -- Format the verb prompt and find which verbs are possible based on this information
              (wordPrompt, matchingVerbs') =
                if includeVerb then
                  (learnShowTamil fullVerb ++ " " ++ definitionString, filteredMatching)
                else
                  (definitionString, matchingVerbs)
              -- Format the full question based on the conjugation
              question =
                "What is the " ++ showConjugation learnShowTamil wordPrompt conjugation ++ "?" ++
                  case verbNote verb of
                    "" -> ""
                    note -> " [note: " ++ note ++ "]"
              -- Find the answers that could match the question
              answers =
                map (conjugate conjugation) matchingVerbs'
            score <- f n question answers
            go g (n + 1) (correct <> score) verbs'

-- | Represents how correct an answer is
data AnswerScore
  = Correct
  | CorrectButUncommon ChoiceString
  | IncorrectButClose
  | Incorrect

instance Semigroup AnswerScore where
  Correct <> _ = Correct
  _ <> Correct = Correct
  a@(CorrectButUncommon _) <> _ = a
  _ <> b@(CorrectButUncommon _) = b
  IncorrectButClose <> _ = IncorrectButClose
  _ <> IncorrectButClose = IncorrectButClose
  _ <> _ = Incorrect

instance Monoid AnswerScore where
  mempty = Incorrect

-- | Scores an answer based on the correct conjugations for a single verb
scoreAnswer :: (TamilLetter -> TamilLetter -> Bool) -> TamilString -> ChoiceString -> AnswerScore
scoreAnswer check tamil (ChoiceString c o) =
  foldMap (go Correct) c <>
    foldMap (go $ CorrectButUncommon $ ChoiceString c []) o
  where
    go correct answer
      | isSimilar check tamil answer = correct
      | isWithinOneEdit pLookup tamil answer = IncorrectButClose
      | otherwise = Incorrect

-- | Scores an answer based on the correct conjugations for all matching verbs
scoreAll :: (TamilLetter -> TamilLetter -> Bool) -> TamilString -> [ChoiceString] -> AnswerScore
scoreAll _ (TamilString []) _ = Incorrect
scoreAll check tamil answers = foldMap (scoreAnswer check tamil) answers

-- | Repeatedly prompts the user until they enter a valid 'TamilString'
getTamil :: IO TamilString
getTamil = do
  putStr "> "
  hFlush stdout
  answer <- getLineOrExit
  case answer of
    (':' : 'q' : _) ->
      exitSuccess
    _ ->
      case parseTamil answer of
        Left err -> do
          putStrLn $ "That isn't valid Tamil (" ++ err ++ ")."
          getTamil
        Right tamil ->
          return tamil

-- | Prompts the user for an answer and then provides feedback based on how correct it was
checkAnswer :: Bool -> (TamilLetter -> TamilLetter -> Bool) -> (TamilString -> String) -> [ChoiceString] -> IO Bool
checkAnswer mayRetry check showTamil answers = do
  tamil <- getTamil
  let showChoices = showUsing showTamil
  case scoreAll check tamil answers of
    Correct -> do
      putStrLn $ "Correct!"
      return True
    CorrectButUncommon (ChoiceString [moreCommon] _) -> do
      putStrLn $ "Correct! But " ++ showTamil moreCommon ++ " is more common."
      return True
    CorrectButUncommon moreCommon -> do
      putStrLn $ "Correct! But these are more common: " ++ showChoices moreCommon
      return True
    IncorrectButClose | mayRetry -> do
      putStrLn "That's close but not quite right. Try again."
      checkAnswer False check showTamil answers
    _ -> do
      let
        header =
          case tamil of
            TamilString [] -> ""
            _ -> "Incorrect. "
        msg =
          case map hide answers of
            [ChoiceString [answer] _] ->
              "The answer was " ++ showTamil answer ++ "."
            [answer] ->
              "The answer was any of " ++ showChoices answer ++ "."
            answers ->
              "There are multiple verbs that would be correct:" ++
                concatMap (\answer -> "\n" ++ showChoices answer) answers
      putStrLn $ header ++ msg
      return False

-- | Generate questions about verb conjugations using the provided settings
startLearn :: LearnSettings -> String -> IO ()
startLearn settings config = do
  config <-
    case parseConfig config of
      Left err -> do
        hPutStrLn stderr $ "error: " ++ err
        exitFailure
      Right config ->
        return config
  let
    showTamil = learnShowTamil settings
    formatAnswers answers =
      intercalate "; " $ map (showUsing showTamil) $ map hide answers
  case learnOutput settings of
    Nothing -> do
      correct <-
        getSum <$> generateQuestions settings config \n q a -> do
          putStrLn $ show n ++ ". " ++ q
          correct <- checkAnswer (not $ learnStrict settings) (learnCheck settings) showTamil a
          putStrLn ""
          return $ Sum if correct then 1 else 0
      let
        total = learnCount settings
        -- Find the percentage rounded to the nearest whole number using integer division tricks
        percent = ((correct * 100) + (total `quot` 2)) `quot` total
      putStrLn $ "Score: " ++ show correct ++ "/" ++ show total ++ " (" ++ show percent ++ "%)"
    Just (hq, ha) -> do
      generateQuestions settings config \n q a -> do
        hPutStrLn hq $ show n ++ ". " ++ q
        hPutStrLn ha $ show n ++ ". " ++ formatAnswers a
      hClose hq
      hClose ha

