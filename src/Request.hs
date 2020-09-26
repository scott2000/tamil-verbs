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
  | TRRelative
  | TRNoun
  | TRAdverb
  | TRInfinitive
  | TRConditional
  | TRCommand
  deriving Eq

instance Show TypeRequest where
  show = \case
    TRAdjective   -> "adjective"
    TRRelative    -> "relative"
    TRNoun        -> "noun"
    TRAdverb      -> "adverb"
    TRInfinitive  -> "infinitive"
    TRConditional -> "conditional"
    TRCommand     -> "command"

data TenseRequest
  = TRPast
  | TRPresent
  | TRFuture
  | TRHabitual
  | TRClassical
  deriving Eq

instance Show TenseRequest where
  show = \case
    TRPast      -> "past"
    TRPresent   -> "present"
    TRFuture    -> "future"
    TRHabitual  -> "habitual"
    TRClassical -> "classical"

data TamilOrLatin
  = TRTamil
  | TRLatin
  deriving Eq

instance Show TamilOrLatin where
  show = \case
    TRTamil -> "tamil"
    TRLatin -> "latin"

data ConjugationRequest = ConjugationRequest
  { crNegative :: Bool
  , crRespectful :: Bool
  , crType :: Maybe TypeRequest
  , crTense :: Maybe TenseRequest
  , crSubject :: Maybe Subject
  , crFormat :: Maybe TamilOrLatin
  , crGuess :: Bool
  , crAlt :: Bool
  , crError :: Bool }

getConjugations :: ConjugationRequest -> Verb -> [Conjugation]
getConjugations cr verb =
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
          | defective -> Third $ Irrational Adhu
          | otherwise -> Naan
    thirdSubject =
      case crSubject cr of
        Just (Third subject) -> subject
        _ -> Avar
    parseNegative =
      case crType cr of
        Just TRAdjective ->
          [Negative NegativeAdjective]
        Just TRRelative ->
          [Negative $ NegativeRelative thirdSubject]
        Just TRNoun ->
          [Negative NegativeNoun]
        Just TRAdverb ->
          [Negative NegativeAdverb]
        Just TRInfinitive ->
          [Negative NegativePastPresent]
        Just TRConditional ->
          [Negative NegativeConditional]
        Just TRCommand ->
          [Negative $ NegativeCommand False]
        Nothing ->
          case crTense cr of
            Just TRFuture ->
              [Negative $ NegativeFuture subject]
            Just TRHabitual ->
              [Negative NegativeHabitual]
            Just TRClassical ->
              [Negative $ NegativeClassical subject]
            _ ->
              case crSubject cr of
                Just subject ->
                  [Negative $ NegativeFuture subject]
                Nothing ->
                  [Negative NegativePastPresent]
    parsePositive =
      case crType cr of
        Just TRAdjective ->
          map (Positive . Adjective) tenses
        Just TRRelative ->
          map (\tense -> Positive $ Relative tense thirdSubject) tenses
        Just TRNoun ->
          [Positive Noun]
        Just TRAdverb ->
          [Positive Adverb]
        Just TRInfinitive -> do
          [Infinitive]
        Just TRConditional ->
          [Positive Conditional]
        Just TRCommand ->
          [Positive $ Command False]
        Nothing
          | not defective, Nothing <- crTense cr, Nothing <- crSubject cr ->
            finite ++ [Positive Adverb, Infinitive]
          | otherwise ->
            finite
      where
        finite =
          map (\tense -> Positive $ Finite tense subject) tenses
        tenses =
          case crTense cr of
            Just TRPast -> [Past]
            Just TRPresent -> [Present]
            Just _ -> [Future]
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
      , crFormat = Nothing
      , crGuess = False
      , crAlt = False
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
    ConjugationRequest { crRespectful = True, crSubject = Just subject } ->
      case makeRespectful subject of
        Just subject ->
          return request { crSubject = Just subject}
        Nothing -> do
          hPutStrLn stderr $ "error: subject cannot be made respectful: " ++ show (tamilShow subject)
          return request { crError = True }
    ConjugationRequest { crRespectful = True } -> do
      hPutStrLn stderr "error: only commands can be made respectful"
      return request { crError = True }
    ConjugationRequest { crNegative = False, crTense = Just TRClassical } -> do
      hPutStrLn stderr "error: classical can only be used with the negative"
      return request { crError = True }
    ConjugationRequest { crType = Just ty, crTense = Just TRClassical } -> do
      hPutStrLn stderr $ "error: " ++ show ty ++ " cannot be made classical"
      return request { crError = True }
    ConjugationRequest { crType = Just TRRelative, crSubject = Just (Third _) } ->
      return request
    ConjugationRequest { crType = Just TRRelative, crSubject = Just _ } -> do
      hPutStrLn stderr "error: only third-person subjects can be used with relative nouns"
      return request { crError = True }
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
        "alternatives" ->
          return cr { crAlt = True }
        "alternative" ->
          return cr { crAlt = True }
        "adjective" ->
          updateType TRAdjective
        "relative" ->
          updateType TRRelative
        "noun" ->
          updateType TRNoun
        "adverb" ->
          updateType TRAdverb
        "infinitive" ->
          updateType TRInfinitive
        "conditional" ->
          updateType TRConditional
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
        "classical" ->
          updateTense TRClassical
        "classic" ->
          updateTense TRClassical
        "ancient" ->
          updateTense TRClassical
        "tamil" ->
          updateFormat TRTamil
        "latin" ->
          updateFormat TRLatin
        "english" ->
          updateFormat TRLatin
        "neg" ->
          return cr { crNegative = True }
        "not" ->
          return cr { crNegative = True }
        "no" ->
          return cr { crNegative = True }
        "resp" ->
          return cr { crRespectful = True }
        "res" ->
          return cr { crRespectful = True }
        "alt" ->
          return cr { crAlt = True }
        "adj" ->
          updateType TRAdjective
        "rel" ->
          updateType TRRelative
        "adv" ->
          updateType TRAdverb
        "avp" ->
          updateType TRAdverb
        "inf" ->
          updateType TRInfinitive
        "cond" ->
          updateType TRConditional
        "if" ->
          updateType TRConditional
        "com" ->
          updateType TRCommand
        "pres" ->
          updateTense TRPresent
        "fut" ->
          updateTense TRFuture
        "hab" ->
          updateTense TRHabitual
        "class" ->
          updateTense TRClassical
        "old" ->
          updateTense TRClassical
        "tam" ->
          updateFormat TRTamil
        "lat" ->
          updateFormat TRLatin
        "eng" ->
          updateFormat TRLatin
        "i" ->
          updateSubject Naan
        "we" ->
          updateSubject Naam
        "you" ->
          updateSubject Nee
        "he" ->
          updateSubject $ Third Avan
        "she" ->
          updateSubject $ Third Aval
        "they" ->
          updateSubject $ Third Avargal
        "it" ->
          updateSubject $ Third $ Irrational Adhu
        "that" ->
          updateSubject $ Third $ Irrational Adhu
        "this" ->
          updateSubject $ Third $ Irrational Adhu
        "these" ->
          updateSubject $ Third $ Irrational Avai
        "those" ->
          updateSubject $ Third $ Irrational Avai
        other ->
          case
            case parseTamil other of
              Left _ -> Nothing
              Right subject ->
                let
                  normalized =
                    case normalize subject of
                      TamilString str | last str == Vowel (I Short) ->
                        -- Replace idhu and edhu with adhu
                        TamilString $ init str ++ [Vowel (A Short)]
                      other -> other
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
        updateFormat = update crFormat \crFormat -> cr { crFormat }
        updateSubject = update crSubject \crSubject -> cr { crSubject }
        update get put new = do
          case get cr of
            Just old | old /= new -> do
              hPutStrLn stderr $ "warning: " ++ show new ++ " overrides " ++ show old
            _ ->
              return ()
          return $ put $ Just new

guessNoInfo :: [TamilLetter] -> [(String, Verb)]
guessNoInfo str =
  case reducedStr of
    -- These are very hard to guess due to adding euphonic U
    Vowel (U Short) : Consonant (Medium _) : _ -> []
    Consonant (Medium Y) : _ ->
      [basicClass $ Class1 Weak]
    Consonant c : Vowel v : _ | mayDouble c, not $ isShortishVowel v ->
      [basicClass $ Class1 Strong]
    _
      | isShortish reducedRoot ->
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
    prefixRoot = TamilString prefixStr
    (reducedStr, prefixStr) =
      let unsplit = (str, []) in
      case
        case str of
          Consonant c1 : Vowel v : Consonant c2 : prefix ->
            Just (c2, ([Consonant c1, Vowel v, Consonant c2], prefix))
          Vowel v1 : Consonant c1 : Vowel v2 : Consonant c2 : prefix ->
            Just (c2, ([Vowel v1, Consonant c1, Vowel v2, Consonant c2], prefix))
          _ ->
            Nothing
      of
        Nothing -> unsplit
        Just (firstConsonant, split@(_, prefix)) ->
          case firstConsonant of
            Hard h ->
              case prefix of
                Consonant (Hard h') : rest | h' == h ->
                  -- Make sure hard consonant was correctly doubled if it was doubled
                  case rest of
                    Vowel (I Short) : Consonant _ : _ -> split
                    Vowel (U Short) : Consonant x : Consonant y : _ | x == y -> split
                    _ -> unsplit
                Vowel (U Short) : Consonant (Hard x) : Consonant (Hard y) : _ | x == y -> unsplit
                Vowel (U Short) : Consonant _ : _ -> split
                _ -> unsplit
            _ ->
              case prefix of
                Vowel (I Short) : Consonant _ : _ -> split
                Vowel (U Short) : Consonant _ : _ -> split
                _ -> unsplit
    basicClass c =
      ( show c
      , defaultVerb
        { verbRoot = reducedRoot
        , verbPrefix = prefixRoot
        , verbClass = c } )
    defective2W =
      ( show $ Class2 Weak
      , defaultVerb
          { verbRoot = reducedRoot
          , verbPrefix = prefixRoot
          , verbDefective = True
          , verbClass = Class2 Weak } )

guess :: VerbList -> TamilString -> [(String, Verb)]
guess verbList basicRoot =
  case go $ sortOn (\(TamilString root, _) -> -(length root)) combinedList of
    [] -> guessNoInfo reducedStr
    verbs -> verbs
  where
    combinedList =
      HashMap.toList (byRoot verbList) ++ HashMap.toList (byRoot irregularVerbs)
    reducedStr =
      case untamil basicRoot of
        Vowel (U Short) : Consonant (Hard K) : rest@(Vowel v : _) | not $ isShortishVowel v -> rest
        Vowel (U Short) : Consonant c : rest@(Consonant o : _) | c == o, mayDouble c -> rest
        Vowel (U Short) : rest@(Consonant c : _) | mayDouble c -> rest
        str -> str
    go [] = []
    go ((root, verbs):rest) =
      case stripSuffix root basicRoot of
        Nothing -> go rest
        Just rootPrefix ->
          let
            updateVerb v =
              ( show (verbClass v)
              , v { verbPrefix = rootPrefix `append` verbPrefix v } )
          in
            map updateVerb verbs

lookupVerb :: VerbList -> (TamilString -> String) -> Bool -> String -> Either String [(String, Verb)]
lookupVerb verbList showTamil allowGuess word =
  case HashMap.lookup (map toLower word) $ byDefinition verbList of
    Just verbs ->
      Right $ map (\verb -> (showTamil $ suffix (verbPrefix verb) (verbRoot verb), verb)) verbs
    Nothing ->
      let notDefinition = Left "cannot find word with that definition" in
      case parseTamil word of
        Left err ->
          if any isUpper word then
            Left $ show err
          else if any isSpace word then
            notDefinition
          else
            Left $ "not found as definition and not valid as Tamil (" ++ show err ++ ")"
        Right tamil ->
          case HashMap.lookup tamil $ byRoot verbList of
            Just verbs ->
              Right $ map (\verb -> (intercalate ", " $ verbDefinitions verb, verb)) verbs
            Nothing ->
              case findError tamil of
                Just err ->
                  if allowGuess then
                    Left err
                  else
                    notDefinition
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

processRequest :: VerbList -> String -> String -> IO ()
processRequest verbList verb conjugation = do
  let word = stripTo $ unwords $ splitHyphen verb
  request <- parseConjugationRequest $ splitHyphen conjugation
  let
    showTamil =
      case crFormat request of
        Nothing -> show
        Just TRTamil -> toTamil
        Just TRLatin -> toLatin
    showChoices =
      if crAlt request then
        showUsing showTamil
      else
        showUsing showTamil . hide
  when (not $ crError request)
    case lookupVerb verbList showTamil (crGuess request) word of
      Left err ->
        hPutStrLn stderr $ "error: " ++ err
      Right [(_, verb)] ->
        forM_ (getConjugations request verb) \conjugation ->
          putStrLn $ showChoices $ conjugate conjugation verb
      Right verbs ->
        let
          sortedVerbs =
            sortOn (\(_, v) -> v) verbs
          conjugations =
            map withConjugation sortedVerbs
          withConjugation (header, verb) =
            (header, verb, getConjugations request verb)
          allSingle =
            all isSingle conjugations
          isSingle = \case
            (_, _, _:_:_) -> False
            _ -> True
        in
          forM_ conjugations \(header, verb, conjugations) ->
            case conjugations of
              [] -> return ()
              [conjugation] | allSingle ->
                putStrLn $ header ++ ": " ++ showChoices (conjugate conjugation verb)
              _ -> do
                putStrLn $ header ++ ":"
                forM_ conjugations \conjugation ->
                  putStrLn $ "  " ++ showChoices (conjugate conjugation verb)

irregularVerbs :: VerbList
irregularVerbs = foldl' (flip addVerb) emptyVerbList
  [ defaultVerb
      { verbRoot = "pODu"
      , verbClass = Class1 Weak }
  , defaultVerb
      { verbRoot = "saa"
      , verbAdverb = Just "settu"
      , verbClass = Class1 Weak }
  , defaultVerb
      { verbRoot = "azhu"
      , verbClass = Class1 Weak }
  , defaultVerb
      { verbRoot = "kal"
      , verbClass = Class1 Strong }
  , defaultVerb
      { verbRoot = "vil"
      , verbClass = Class1 Strong }
  , defaultVerb
      { verbRoot = "eDu"
      , verbClass = Class1 Strong }
  , defaultVerb
      { verbRoot = "paar"
      , verbClass = Class1 Strong }
  , defaultVerb
      { verbRoot = "thaakkuRu"
      , verbAdverb = Just "thaakkuNDu"
      , verbClass = Class2 Weak }
  , defaultVerb
      { verbRoot = "thaa"
      , verbAdverb = Just "thandhu"
      , verbStem = Just "tharu"
      , verbRespectfulCommandRoot = Just "thaaru"
      , verbClass = Class2 Weak }
  , defaultVerb
      { verbRoot = "vaa"
      , verbAdverb = Just "vandhu"
      , verbStem = Just "varu"
      , verbRespectfulCommandRoot = Just "vaaru"
      , verbClass = Class2 Weak }
  , defaultVerb
      { verbRoot = "pOdhu"
      , verbDefective = True
      , verbAdverb = Just $ ChoiceString ["pOndhu", "pOrndhu"] []
      , verbClass = Class2 Weak }
  , defaultVerb
      { verbRoot = "kaaN"
      , verbAdverb = Just "kaNDu"
      , verbClass = Class2 Weak }
  , defaultVerb
      { verbRoot = "en"
      , verbInfinitiveRoot = Just $ ChoiceString ["ennu"] ["enu"]
      , verbClass = Class2 Weak }
  , defaultVerb
      { verbRoot = "agal"
      , verbClass = Class2 Weak }
  , defaultVerb
      { verbRoot = "neeL"
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
      , verbClass = Class3 }
  , defaultVerb
      { verbRoot = "paNNu"
      , verbClass = Class3 }
  , defaultVerb
      { verbRoot = "vaar"
      , verbStem = Just "vaaru"
      , verbClass = Class3 } ]

