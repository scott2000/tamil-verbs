-- | Functions for processing a request for a conjugation from a user
module Request where

import TamilString
import Verb
import Conjugation

import Control.Monad

import Data.List
import Data.Char

import System.IO

import qualified Data.Set as Set
import qualified Data.HashMap.Strict as HashMap

-- | A type of conjugation that can be requested
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

-- | A type of tense (or related item) that can be requested
data TenseRequest
  = TRPast
  | TRPresent
  | TRFuture
  | TRClassical
  deriving Eq

instance Show TenseRequest where
  show = \case
    TRPast      -> "past"
    TRPresent   -> "present"
    TRFuture    -> "future"
    TRClassical -> "classical"

-- | An alphabet that can be used to print the result
data TamilOrLatin
  = TRTamil
  | TRLatin
  deriving Eq

instance Show TamilOrLatin where
  show = \case
    TRTamil -> "tamil"
    TRLatin -> "latin"

-- | A parsed request from the user
data ConjugationRequest = ConjugationRequest
  { crError :: Bool       -- ^ If true, there has been a fatal error in parsing
  , crGuess :: Bool       -- ^ If true, allow guessing when a verb isn't known
  , crAlt :: Bool         -- ^ If true, show alternative conjugations
  , crNegative :: Bool    -- ^ If true, a 'NegativeConjugation' should be used
  , crRespectful :: Bool  -- ^ If true, a command should be made respectful
  , crType :: Maybe TypeRequest
  , crTense :: Maybe TenseRequest
  , crSubject :: Maybe Subject
  , crFormat :: Maybe TamilOrLatin }

-- | Get the set of 'Conjugation's that should be displayed for a 'ConjugationRequest'
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
    inanimate = verbInanimate verb
    subject =
      case crSubject cr of
        Just subject ->
          subject
        Nothing
          | defective -> Third $ Irrational Adhu
          | inanimate -> Third $ Irrational Adhu
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

-- | Parse a set of keywords into a 'ConjugationRequest'
parseConjugationRequest :: [String] -> IO ConjugationRequest
parseConjugationRequest parts = do
  let
    defaultRequest = ConjugationRequest
      { crError = False
      , crGuess = False
      , crAlt = False
      , crNegative = False
      , crRespectful = False
      , crType = Nothing
      , crTense = Nothing
      , crSubject = Nothing
      , crFormat = Nothing }
  request <- foldM go defaultRequest parts
  case request of
    ConjugationRequest { crNegative = True, crType = Just TRInfinitive } -> do
      hPutStrLn stderr "error: infinitives cannot be negative"
      return request { crError = True }
    ConjugationRequest { crRespectful = True, crType = Nothing, crTense = Nothing, crSubject = Nothing } ->
      return request { crType = Just TRCommand }
    ConjugationRequest { crRespectful = True, crType = Just TRCommand } ->
      continue request
    ConjugationRequest { crRespectful = True, crSubject = Just subject } ->
      case makeRespectful subject of
        Just subject ->
          continue request { crSubject = Just subject}
        Nothing -> do
          hPutStrLn stderr $ "error: subject cannot be made respectful: " ++ show (tamilShow subject)
          return request { crError = True }
    ConjugationRequest { crRespectful = True } -> do
      hPutStrLn stderr "error: only commands can be made respectful"
      return request { crError = True }
    _ ->
      continue request
  where
    continue request =
      case request of
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

-- | Given some root and no other information, guess the verb's conjugation
guessNoInfo :: TamilString -> [(String, Verb)]
guessNoInfo (TamilString str) =
  case reducedStr of
    -- These are very hard to guess due to adding euphonic U
    Consonant (Medium Y) : _ ->
      [basicClass $ Class1 Weak]
    Consonant c : Vowel v : _ | mayDouble c, not $ isShortishVowel v ->
      [basicClass $ Class1 Strong]
    Vowel (I Short) : Consonant (Medium V) : _ ->
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
    -- Try to split the string if there's a word boundary to split at before the last 2 letters
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
                -- Make sure hard consonant was correctly doubled if it was doubled
                Consonant (Hard h') : rest | h' == h ->
                  case rest of
                    -- Accusative case
                    Vowel Ai : Consonant _ : _ -> split
                    -- Infinitive
                    Vowel (A Short) : Consonant c : _ | infinitiveLetter c -> split
                    -- Class 3 adverb
                    Vowel (I Short) : Consonant _ : _ -> split
                    Consonant (Medium Y) : _ -> split
                    -- Class 1 adverb, dative case, etc.
                    Vowel (U Short) : Consonant x : Consonant y : _ | x == y -> split
                    Vowel (U Short) : Consonant (Hard _) : Consonant (Hard _) : _ -> split
                    _ -> unsplit
                -- This isn't an adverb or dative case since it wasn't doubled
                Vowel (U Short) : Consonant x : Consonant y : _ | x == y -> unsplit
                Vowel (U Short) : Consonant (Hard _) : Consonant (Hard _) : _ -> unsplit
                -- Class 2 adverb
                Vowel (U Short) : Consonant _ : _ -> split
                _ -> unsplit
            -- These can't start words, so they definitely won't split
            Medium LRetroflex -> unsplit
            Medium LAlveolar -> unsplit
            Medium R -> unsplit
            Medium Zh -> unsplit
            _ ->
              -- Check if this could be a good word boundary, but be more strict since there's no way to check doubling
              case prefix of
                -- Accusative case
                Vowel Ai : Consonant _ : _ -> split
                -- Infinitive
                Vowel (A Short) : Consonant c : _ | infinitiveLetter c -> split
                -- Class 1 adverb
                Vowel (U Short) : Consonant (Hard h) : Consonant (Hard h') : _ | h == h' -> split
                -- Class 2 adverb
                Vowel (U Short) : Consonant (Hard h) : Consonant (Soft s) : _ | s == getPaired h -> split
                -- Class 3 adverb
                Vowel (I Short) : Consonant _ : _ -> split
                Consonant (Medium Y) : _ -> split
                _ -> unsplit
    infinitiveLetter = \case
      Hard K          -> True
      Medium _        -> True
      Soft NRetroflex -> True
      Soft NAlveolar  -> True
      _               -> False
    basicClass c =
      ( show c
      , defaultVerb
        { verbClass = c
        , verbRoot = reducedRoot
        , verbPrefix = prefixRoot } )
    defective2W =
      ( show $ Class2 Weak
      , defaultVerb
          { verbClass = Class2 Weak
          , verbRoot = reducedRoot
          , verbPrefix = prefixRoot
          , verbDefective = True } )

-- | Given some root, try to guess how to conjugate the verb, possibly making assumptions due to the form of the root
guess :: Bool -> VerbList -> TamilString -> [(String, Verb)]
guess allowNoInfoGuess verbList basicRoot =
  case go $ sortOn (\(TamilString root, _) -> -(length root)) combinedList of
    [] | allowNoInfoGuess -> guessNoInfo $ TamilString reducedStr
    verbs -> verbs
  where
    combinedList =
      concatMap (HashMap.toList . byRoot) [verbList, defaultVerbList, irregularVerbs]
    reducedStr =
      case untamil basicRoot of
        Vowel (U Short) : Consonant (Hard K) : rest@(Vowel v : _) | not $ isShortishVowel v -> rest
        Vowel (U Short) : Consonant c : rest@(Consonant o : _) | c == o, mayDouble c -> rest
        Vowel (U Short) : rest@(Consonant c : _) | mayDouble c -> rest
        Vowel (U Short) : rest@(Consonant (Medium R) : _) -> rest
        str -> str
    go [] = []
    go ((root, verbs):rest) =
      case stripSuffix root basicRoot of
        Nothing -> go rest
        Just rootPrefix ->
          let
            updateVerb v =
              ( show $ verbClass v
              , v { verbPrefix = rootPrefix `append` verbPrefix v
                  , verbNote = "" } )
          in
            map updateVerb $ Set.toList verbs

-- | Checks if a 'TamilString' looks like a normal Tamil verb
looksLikeVerb :: TamilString -> Bool
looksLikeVerb (TamilString str) =
  case str of
    Consonant c : _ ->
      case c of
        Soft NRetroflex   -> True
        Soft NAlveolar    -> True
        Medium Y          -> True
        Medium R          -> True
        Medium LAlveolar  -> True
        Medium LRetroflex -> True
        Medium Zh         -> True
        _                 -> False
    Vowel v : _ ->
      case v of
        A Short -> True
        I Short -> True
        U Short -> True
        Ai      -> True
        _       -> False
    _ -> False

-- | Tries to look up a search in a 'VerbList' given a function to show Tamil words and optionally guesses if needed
lookupVerb :: VerbList -> (TamilString -> String) -> Bool -> String -> Either String [(String, Verb)]
lookupVerb verbList showTamil allowGuess word =
  case HashMap.lookup (map toLower word) $ byDefinition verbList of
    Just verbs ->
      Right $ map (\verb -> (getReturnedRoot showTamil verb, verb)) $ Set.toList verbs
    Nothing ->
      let notDefinition = Left "cannot find word with that definition" in
      case parseTamil word of
        Left err ->
          if any isUpper word then
            Left err
          else if any isSpace word then
            notDefinition
          else
            Left $ "not found as definition and not valid as Tamil (" ++ err ++ ")"
        Right tamil ->
          case HashMap.lookup tamil $ byRoot verbList of
            Just verbs ->
              Right $ map (\verb -> (getReturnedDefinitions verb, verb)) $ Set.toList verbs
            Nothing ->
              case validateTamil tamil of
                Left err ->
                  if allowGuess then
                    Left err
                  else
                    notDefinition
                Right () ->
                  if allowGuess then
                    Right $ guess True verbList tamil
                  else
                    Left $ "verb not found" ++
                      let
                        normalized = normalize tamil
                        suggestions = filter ((normalized ==) . normalize) $ HashMap.keys $ byRoot verbList
                      in
                        case take 3 $ sort suggestions of
                          [] ->
                            let
                              hasUpperCaseOrTamil = any (not . isAsciiLower) word
                              endsWithNonEnglish =
                                case untamil tamil of
                                  Consonant (Medium LAlveolar) : _  -> True
                                  Consonant (Medium LRetroflex) : _ -> True
                                  Vowel (U Short) : _ -> True
                                  Vowel (I Short) : _ -> True
                                  _ -> False
                              isSpecial = \case
                                Consonant (Soft Ny) -> True
                                Consonant (Medium Zh) -> True
                                Vowel (A Long) -> True
                                Vowel (E Long) -> True
                                Vowel (O Long) -> True
                                Vowel Au -> True
                                _ -> False
                              looksLikeTamil =
                                hasUpperCaseOrTamil || endsWithNonEnglish || any isSpecial (untamil tamil)
                              looksLikeTamilVerb =
                                looksLikeTamil && looksLikeVerb tamil
                            in
                              -- Only allow no-info guessing if the word looks like a predictable Tamil verb
                              case guess looksLikeTamilVerb verbList tamil of
                                [] -> ""
                                list@(_ : _ : _) | any (null . verbDefinitions . snd) list ->
                                  -- If there are multiple choices and it wasn't a known word, don't recommend
                                  ""
                                _ ->
                                  " (maybe try again with 'guess'?)"
                          suggestions ->
                            let showFunction = if all isAscii word then toLatin else toTamil in
                            " (did you mean " ++ intercalate " or " (map showFunction suggestions) ++ "?)"

-- | Processes a conjugation request for a given verb with given arguments in a given 'VerbList'
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
      Right [(_, verb)] | not $ headerRequired word verb ->
        forM_ (getConjugations request verb) \conjugation ->
          putStrLn $ showChoices $ conjugate conjugation verb
      Right verbs ->
        forM_ verbs \(header, verb) ->
          case getConjugations request verb of
            [] -> return ()
            conjugations -> do
              putStrLn $ header ++ ":"
              forM_ conjugations \conjugation ->
                putStrLn $ "  " ++ showChoices (conjugate conjugation verb)

-- | A set of known irregular verb conjugations that can be used when guessing
irregularVerbs :: VerbList
irregularVerbs = makeVerbList $ concat
  [ -- Class 1 Weak
    setClass (Class1 Weak)
      [ defaultVerb { verbRoot = "azhu" }
      , defaultVerb
          { verbRoot = "saa"
          , verbAdverb = Just "settu"
          , verbFutureAdhu = Just $ ChoiceString ["saagum"] ["saam"] } ]

    -- Class 1 Strong
  , setClass (Class1 Strong)
      [ defaultVerb { verbRoot = "kal" }
      , defaultVerb { verbRoot = "vil" } ]

    -- Class 2 Weak
  , setClass (Class2 Weak)
      [ defaultVerb { verbRoot = "agal" }
      , defaultVerb { verbRoot = "neeL" }
      , defaultVerb
          { verbRoot = "thaakkuRu"
          , verbAdverb = Just "thaakkuNDu" }
      , defaultVerb
          { verbRoot = "pOdhu"
          , verbDefective = True
          , verbAdverb = Just $ ChoiceString ["pOndhu", "pOrndhu"] [] }
      , defaultVerb
          { verbRoot = "nO"
          , verbAdverb = Just "nondhu" }
      , defaultVerb
          { verbRoot = "vE"
          , verbAdverb = Just "vendhu"
          , verbFutureAdhu = Just "vEgum"
          , verbInfinitiveRoot = Just $ ChoiceString ["vEgu"] ["vEvu"]
          , verbRespectfulCommandRoot = Just $ ChoiceString ["vEgu"] ["vEvu"] }
      , defaultVerb
          { verbRoot = "kaaN"
          , verbAdverb = Just "kaNDu" }
      , defaultVerb
          { verbRoot = "en"
          , verbInfinitiveRoot = Just $ ChoiceString ["enu"] ["ennu"] } ]

        -- Class 3
      , setClass Class3
          [ defaultVerb { verbRoot = "paNNu" }
          , defaultVerb
              { verbRoot = "vaar"
              , verbStem = Just "vaaru" } ] ]
  where
    setClass verbClass =
      map \verb -> verb { verbClass }

