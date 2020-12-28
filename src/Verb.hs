-- | Verb lists for storing information needed to conjugate verbs
module Verb where

import TamilString

import Control.Monad

import Data.String
import Data.List
import Data.Char
import Data.Hashable

import Data.Set (Set)
import qualified Data.Set as Set
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap

-- | Indicates whether a tongue position is used for N or L
data NorL = N | L

-- | Represents different verb endings that have different conjugations
data VerbEnding
  = HardAndU Vallinam
  | LongVowel
  | Retroflex NorL
  | Alveolar NorL
  | RLike
  | OtherEnding

-- | Find the 'VerbEnding' of a 'TamilString'
getEnding :: TamilString -> VerbEnding
getEnding (TamilString str) =
  case str of
    Consonant c : Vowel _ : _ ->
      case c of
        Soft   NRetroflex -> Retroflex N
        Soft   NAlveolar  -> Alveolar  N
        Medium LRetroflex -> Retroflex L
        Medium LAlveolar  -> Alveolar  L
        Medium R          -> RLike
        Medium Zh         -> RLike
        _                 -> OtherEnding
    Vowel (U Short) : Consonant (Hard h) : Vowel _ : _ ->
      HardAndU h
    Vowel v : _ | not $ isShortishVowel v ->
      LongVowel
    _ ->
      OtherEnding

-- | Represents the strength of a verb for the present and future tense conjugations
data Strength
  = Weak
  | Strong
  deriving (Ord, Eq)

-- | Represents the class of a verb for the past tense conjugation
data VerbClass
  = Class1 Strength
  | Class2 Strength
  | Class3
  deriving (Ord, Eq)

instance Show VerbClass where
  show = \case
    Class1 Weak   -> "1W"
    Class1 Strong -> "1S"
    Class2 Weak   -> "2W"
    Class2 Strong -> "2S"
    Class3        -> "3"

-- | Parses the 'VerbClass' in a 'Verb' entry
parseClass :: String -> Either String VerbClass
parseClass = \case
  '1' : s -> Class1 <$> parseStrength s
  '2' : s -> Class2 <$> parseStrength s
  "3"     -> Right Class3
  "3W"    -> Left "expected 3 instead of 3W"
  "3S"    -> Left "class 3 cannot be strong"
  _       -> Left "expected 1W, 1S, 2W, 2S, or 3 for class"
  where
    parseStrength = \case
      "W" -> Right Weak
      "S" -> Right Strong
      _   -> Left "expected W or S for strength"

-- | Remove @"to "@ from the start of verb definitions
stripTo :: String -> String
stripTo (' ' : s) = stripTo s
stripTo ('t' : 'o' : ' ' : s) = stripTo s
stripTo other = other

-- | Like 'parseTamil', but rejects empty strings
parseTamilNonEmpty :: String -> Either String TamilString
parseTamilNonEmpty s =
  case parseTamil s of
    Right (TamilString []) ->
      Left "word cannot be empty"
    other -> other

-- | Parses a 'ChoiceString' for a flag in a 'Verb' entry
parseFlagChoice :: String -> Either String ChoiceString
parseFlagChoice s =
  case map (split ',') $ split ';' s of
    [c] ->
      (`ChoiceString` []) <$> getTamil c
    [c, o] ->
      ChoiceString <$> getTamil c <*> getTamil o
    _ ->
      Left "expected either 1 or 2 sets of choices"
  where
    getTamil = traverse \str -> do
      parsed <- parseTamilNonEmpty str
      checkValid "flag" str parsed
      return parsed

-- | Parses a 'VerbDefinition' with an optional note
parseDefinition :: String -> Either String VerbDefinition
parseDefinition = go
  where
    go [] =
      Right $ VerbDefinition "" ""
    go ('(':rest) =
      (VerbDefinition "") <$> parseNote rest
    go (' ':rest) = do
      def <- go rest
      return $
        case def of
          VerbDefinition def@(_:_) note ->
            VerbDefinition (' ' : def) note
          _ -> def
    go (ch:rest) = do
      checkChar ch
      VerbDefinition def note <- go rest
      return $ VerbDefinition (ch : def) note

    parseNote [] =
      Left "unclosed parentheses in note for definition"
    parseNote [')'] =
      Right ""
    parseNote ('(':_) =
      Left "too many opening parentheses in note for definition"
    parseNote (')':')':_) =
      Left "too many closing parentheses in note for definition"
    parseNote (')':_) =
      Left "note must come at the end of a definition"
    parseNote (' ':rest) =
      (' ' :) <$> parseNote rest
    parseNote (ch:rest) = do
      checkChar ch
      (ch :) <$> parseNote rest

    checkChar ch
      | isAlpha ch = Right ()
      | otherwise =
        Left "verb definitions cannot contain special characters, only [a-z]"

-- | Checks if a 'TamilString' is valid if it wasn't written in Tamil letters
checkValid :: String -> String -> TamilString -> Either String ()
checkValid kind unparsed str
  -- Don't validate consonant clusters if directly written in Tamil
  | all isTamilOrSpace unparsed = return ()
  | otherwise =
    case validateTamil str of
      Left err ->
        Left $ "invalid " ++ kind ++ " '" ++ unparsed ++ "': " ++ err
      other -> other
  where
    isTamilOrSpace x = not (isAscii x) || x == ' '

-- | Parse a 'Verb' entry in a 'VerbList'
parseVerb :: String -> Either String Verb
parseVerb s =
  case split '.' s of
    rootInfo : definitions : flags -> do
      verb <-
        case words rootInfo of
          [class_, root] ->
            getRootInfo class_ "" root
          [class_, prefix, root] ->
            getRootInfo class_ prefix root
          _ ->
            Left "expected 'CLASS [PREFIX] ROOT' for first section of verb"
      verb <-
        case map (stripTo . map toLower . unwords . splitHyphen) $ split ',' definitions of
          [] ->
            Left "expected at least one definition in second section of verb"
          ["???"] ->
            Right verb
          definitions -> do
            verbDefinitions <- nub <$> mapM parseDefinition definitions
            return verb { verbDefinitions }
      foldM addFlag verb $ map words flags
    _ ->
      Left "missing definition for verb"
  where
    getRootInfo class_ prefix root = do
      verbClass <- parseClass class_

      verbPrefix <- parseTamil prefix
      checkValid "prefix" prefix verbPrefix

      verbRoot <- parseTamilNonEmpty root
      checkValid "root" root verbRoot

      checkValid "verb" (prefix ++ " " ++ root) $ suffix verbPrefix verbRoot

      return defaultVerb { verbClass, verbPrefix, verbRoot }
    addFlag verb ["defect"]
      | verbDefective verb = Left "verb already marked defective"
      | verbInanimate verb = Left "defective implies inanimate"
      | otherwise          = Right verb { verbDefective = True }
    addFlag verb ["inanim"]
      | verbDefective verb = Left "defective implies inanimate"
      | verbInanimate verb = Left "verb already marked inanimate"
      | otherwise          = Right verb { verbInanimate = True }
    addFlag _ [flag] =
      Left $ "invalid flag for verb: " ++ flag
    addFlag verb ("note" : rest) =
      case verbNote verb of
        "" -> Right verb
          { verbNote = unwords rest }
        _ ->
          Left "verb already has a note associated with it"
    addFlag verb (key : strParts) = do
      choiceStr@(ChoiceString c u) <- parseFlagChoice $ concat strParts
      let
        removeEnding = replaceEnding ""
        replaceEnding newEnding kind endings =
          ChoiceString <$> mapM go c <*> mapM go u
          where
            go str = foldl1' (<>) $ map (checkEnding str) endings
            checkEnding str ending =
              case stripSuffix ending str of
                Just rest ->
                  Right $ rest `append` newEnding
                Nothing ->
                  Left $ kind ++ " must end in " ++ intercalate " or " (map show endings)
      (old, verb) <-
        case key of
          "adv" ->
            if verbClass verb == Class3 then
              Left $ "adverb cannot be changed for class 3 verbs"
            else do
              removeEnding "adverb" ["u"]
              Right (verbAdverb verb, verb { verbAdverb = Just choiceStr })
          "stem" -> do
            Right (verbStem verb, verb { verbStem = Just choiceStr })
          "adhu" -> do
            removeEnding "future adhu" ["m"]
            Right (verbFutureAdhu verb, verb { verbFutureAdhu = Just choiceStr })
          "inf" -> do
            choiceStr <- replaceEnding "u" "infinitive" ["a"]
            Right (verbInfinitiveRoot verb, verb { verbInfinitiveRoot = Just choiceStr })
          "resp" -> do
            choiceStr <- removeEnding "respectful command" ["ngaL", "m"]
            Right (verbRespectfulCommandRoot verb, verb { verbRespectfulCommandRoot = Just choiceStr })
          _ ->
            Left $ "invalid key: " ++ key
      case old of
        Nothing ->
          Right verb
        Just _ ->
          Left $ "duplicate entry for `" ++ key ++ "`"
    addFlag _ _ =
      Left "verb flag cannot be empty"

-- | Parse a verb list file's contents and return a list of errors and a 'VerbList'
parseAllVerbs :: String -> ([String], VerbList)
parseAllVerbs file =
  go (lines file) (1 :: Int) [] emptyVerbList
  where
    go lines n errs verbs =
      case lines of
        [] ->
          (reverse errs, verbs)
        line:rest ->
          let n' = n + 1 in
          case line of
            "" ->
              go rest n' errs verbs
            '#':_ ->
              go rest n' errs verbs
            _ ->
              case parseVerb line of
                Right verb ->
                  go rest n' errs (addVerb verb verbs)
                Left e ->
                  let err = "syntax error in line " ++ show n ++ ": " ++ e in
                  go rest n' (err:errs) verbs

-- | Represents a definition of a verb
data VerbDefinition = VerbDefinition
  { -- | The definition itself
    vDefinition :: !String
    -- | A note that goes along with the definition
  , vDefinitionNote :: !String }
  deriving Eq

instance IsString VerbDefinition where
  fromString def = VerbDefinition def ""

instance Show VerbDefinition where
  show (VerbDefinition def note) =
    case note of
      "" -> def
      note ->
        def ++ " (" ++ note ++ ")"

-- | A shorthand for @VerbDefinition@
(~#) :: String -> String -> VerbDefinition
(~#) = VerbDefinition

-- | Represents a verb that can be conjugated
data Verb = Verb
  { -- | The class of the verb
    verbClass :: VerbClass
    -- | The basic root of the verb (used for commands)
  , verbRoot :: TamilString
    -- | The prefix to add to every conjugation (used for compound verbs)
  , verbPrefix :: TamilString
    -- | The definitions of the verb in English
  , verbDefinitions :: [VerbDefinition]
    -- | A note associated with a verb
  , verbNote :: String
    -- | 'True' if the verb should be conjugated in the future primarily
  , verbDefective :: Bool
    -- | 'True' if the verb should be conjugated with adhu primarily (implied by defective)
  , verbInanimate :: Bool
    -- | An irregular adverb/past (only for classes 1 and 2)
  , verbAdverb :: Maybe ChoiceString
    -- | An irregular stem for present and future tense
  , verbStem :: Maybe ChoiceString
    -- | An irregular future adhu
  , verbFutureAdhu :: Maybe ChoiceString
    -- | An irregular infinitive root (ends in U, not A)
  , verbInfinitiveRoot :: Maybe ChoiceString
    -- | An irregular respectful command root (ends in U)
  , verbRespectfulCommandRoot :: Maybe ChoiceString }

instance Eq Verb where
  a == b =
    verbClass a == verbClass b
    && verbRoot a == verbRoot b
    && verbPrefix a == verbPrefix b

instance Ord Verb where
  a `compare` b =
    verbClass a `compare` verbClass b
    <> verbRoot a `compare` verbRoot b
    <> verbPrefix a `compare` verbPrefix b

instance Show Verb where
  show v@Verb { .. } =
    show verbClass ++ " " ++ getFormattedRoot v ++ ". " ++ definitions ++ flags
    where
      definitions =
        case verbDefinitions of
          [] -> "???"
          _  -> intercalate ", " $ map show verbDefinitions
      addNote _ "" s = s
      addNote f note s = ". " ++ f ++ " " ++ note ++ s
      addFlag _ False s = s
      addFlag f True s = ". " ++ f ++ s
      addKey _ Nothing s = s
      addKey k (Just c) s = ". " ++ k ++ " " ++ showUsing toLatinIfValid c ++ s
      flags =
        addNote "note" verbNote $
        addFlag "defect" verbDefective $
        addFlag "inanim" verbInanimate $
        addKey "adv" verbAdverb $
        addKey "stem" verbStem $
        addKey "adhu" verbFutureAdhu $
        addKey "inf" ((|+ "a") <$> verbInfinitiveRoot) $
        addKey "resp" ((|+ "ngaL") <$> verbRespectfulCommandRoot) $
        ""

-- | A 'Verb' with no root, definitions, or class
defaultVerb :: Verb
defaultVerb = Verb
  { verbClass = undefined
  , verbRoot = undefined
  , verbPrefix = ""
  , verbDefinitions = []
  , verbNote = ""
  , verbDefective = False
  , verbInanimate = False
  , verbAdverb = Nothing
  , verbStem = Nothing
  , verbFutureAdhu = Nothing
  , verbInfinitiveRoot = Nothing
  , verbRespectfulCommandRoot = Nothing }

-- | Formats the 'Verb' root as it would be shown in a 'VerbList'
getFormattedRoot :: Verb -> String
getFormattedRoot Verb { .. } =
  case verbPrefix of
    TamilString [] ->
      toLatinIfValid verbRoot
    _ ->
      case validateTamil (suffix verbPrefix verbRoot) of
        Left _ ->
          -- If the combined root isn't valid, put it in Tamil
          toTamil verbPrefix ++ " " ++ toTamil verbRoot
        Right _ ->
          toLatinIfValid verbPrefix ++ " " ++
            case toLatinIfValid verbRoot of
              -- Convert initial 's' to 'ch' if the prefix ends in a hard consonant
              ('s':rest) | TamilString (Consonant (Hard _) : _) <- verbPrefix ->
                "ch" ++ rest
              root ->
                root

-- | Gets the root of the 'Verb' with any prefix attached as well
getFullVerb :: Verb -> TamilString
getFullVerb verb =
  suffix (verbPrefix verb) (verbRoot verb)

-- | Checks if a header with definitions is required for a 'Verb' given a user's requested word
headerRequired :: String -> Verb -> Bool
headerRequired requested Verb { verbDefinitions, verbNote } =
  case verbNote of
    "" -> any hasMatchingNote verbDefinitions
    _  -> any hasMatching     verbDefinitions
  where
    hasMatchingNote VerbDefinition { vDefinition, vDefinitionNote } =
      not (null vDefinitionNote) && vDefinition == requested
    hasMatching VerbDefinition { vDefinition } =
      vDefinition == requested

-- | Formats the 'Verb' root to be displayed to the user, including any notes if necessary
getReturnedRoot :: (TamilString -> String) -> Verb -> String
getReturnedRoot showTamil verb =
  showTamil (getFullVerb verb) ++
    case verb of
      Verb { verbDefinitions = [VerbDefinition { vDefinitionNote = "" }], verbNote = "" } ->
        -- Since there is a single definition with no notes, it is unnecessary to print the definitions
        ""
      _ ->
        " - " ++ getReturnedDefinitions verb

-- | Formats the definitions of a 'Verb', including any notes
getReturnedDefinitions :: Verb -> String
getReturnedDefinitions Verb { verbDefinitions, verbNote } =
  intercalate ", " (map show verbDefinitions) ++
    case verbNote of
      "" -> ""
      note -> " [" ++ note ++ "]"

-- | Gets the possible roots for a 'Verb' (including alternative forms)
getRoot :: Verb -> ChoiceString
getRoot verb =
  let root = verbRoot verb in
  case (verbClass verb, getEnding root) of
    (Class2 Weak, RLike) ->
      ChoiceString [root] [root `append` "u"]
    (_, LongVowel) | Nothing <- verbStem verb ->
      ChoiceString [root] [root `append` "gu"]
    _ ->
      if endsInDoublingConsonant root then
        ChoiceString [root] [suffix root "u"]
      else
        common root

-- | Gets the 'Strength' of a 'Verb'
getStrength :: Verb -> Strength
getStrength verb =
  case verbClass verb of
    Class1 s -> s
    Class2 s -> s
    Class3   -> Weak

-- | A list of 'Verb's sorted in various ways to make searching faster
data VerbList = VerbList
  { -- | All verbs in the verb list (not including children)
    allVerbs :: !(Set Verb)
    -- | Verbs indexed by the verb root
  , byRoot :: !(HashMap TamilString (Set Verb))
    -- | Verbs indexed by the verb definition
  , byDefinition :: !(HashMap String (Set Verb)) }

-- | An empty 'VerbList' with no verbs
emptyVerbList :: VerbList
emptyVerbList = VerbList
  { allVerbs = Set.empty
  , byRoot = HashMap.empty
  , byDefinition = HashMap.empty }

-- | Add a 'Verb' to a 'VerbList'
addVerb :: Verb -> VerbList -> VerbList
addVerb basicVerb verbList@VerbList { allVerbs, byRoot, byDefinition }
  | v `Set.member` allVerbs = verbList
  | otherwise = VerbList
    { allVerbs = Set.insert v allVerbs
    , byRoot = withAllNames
    , byDefinition = foldr insertVerb byDefinition definitions }
  where
    v = basicVerb
      { verbRoot = convertInitialN $ verbRoot basicVerb
      , verbPrefix = convertInitialN $ verbPrefix basicVerb }

    definitions =
      map vDefinition (verbDefinitions v)

    withAllNames =
      foldr insertVerb byRoot $ allChoices allNames
    allNames =
      common (verbPrefix v) |+| (getRoot v <> demote stem)
    stem =
      case verbStem v of
        Nothing ->
          ChoiceString [] []
        Just stem ->
          stem

    insertVerb :: (Eq k, Hashable k) => k -> HashMap k (Set Verb) -> HashMap k (Set Verb)
    insertVerb = HashMap.alter \case
      Nothing -> Just $ Set.singleton v
      Just vs -> Just $ Set.insert v vs

-- | Make a 'VerbList' from a list of verbs
makeVerbList :: [Verb] -> VerbList
makeVerbList = foldl' (flip addVerb) emptyVerbList

-- | The default list of verbs to be used when no verb list is provided
defaultVerbList :: VerbList
defaultVerbList = makeVerbList $ concat
  [ -- Class 1 Weak
    setClass (Class1 Weak)
      [ defaultVerb
          { verbRoot = "saappiDu"
          , verbDefinitions = ["eat"] }
      , defaultVerb
          { verbRoot = "paDu"
          , verbDefinitions = ["experience", "undergo", "suffer"] }
      , defaultVerb
          { verbRoot = "pODu"
          , verbDefinitions = ["drop", "put", "place"] }
      , defaultVerb
          { verbRoot = "viDu"
          , verbDefinitions = ["let go", "release"] }
      , defaultVerb
          { verbRoot = "sey"
          , verbDefinitions = ["do", "make"] } ]

    -- Class 1 Strong
  , setClass (Class1 Strong)
      [ defaultVerb
          { verbRoot = "kEL"
          , verbDefinitions = ["ask", "listen", "hear"] }
      , defaultVerb
          { verbRoot = "eDu"
          , verbDefinitions = ["take", "get"] }
      , defaultVerb
          { verbRoot = "kuDi"
          , verbDefinitions = ["drink"] }
      , defaultVerb
          { verbRoot = "koDu"
          , verbDefinitions = ["give"] }
      , defaultVerb
          { verbRoot = "paDi"
          , verbDefinitions = ["study", "read"] }
      , defaultVerb
          { verbRoot = "paDu"
          , verbDefinitions = ["lie down", "go to bed"] }
      , defaultVerb
          { verbRoot = "paar"
          , verbDefinitions = ["look", "see"] }
      , defaultVerb
          { verbRoot = "vai"
          , verbDefinitions = ["put", "place", "set down"] }
      , defaultVerb
          { verbRoot = "muDi"
          , verbDefinitions = ["finish", "complete"] }
      , defaultVerb
          { verbRoot = "uDai"
          , verbDefinitions = ["break" ~# "transitive"]} ]

    -- Class 2 Weak
  , setClass (Class2 Weak)
      [ defaultVerb
          { verbRoot = "thaa"
          , verbDefinitions = ["give"]
          , verbAdverb = Just "thandhu"
          , verbStem = Just "tharu"
          , verbRespectfulCommandRoot = Just "thaaru" }
      , defaultVerb
          { verbRoot = "vaa"
          , verbDefinitions = ["come"]
          , verbAdverb = Just "vandhu"
          , verbStem = Just "varu"
          , verbRespectfulCommandRoot = Just "vaaru" }
      , defaultVerb
          { verbRoot = "koL"
          , verbDefinitions = ["have", "hold"] }
      , defaultVerb
          { verbRoot = "kol"
          , verbDefinitions = ["kill"] }
      , defaultVerb
          { verbRoot = "chel"
          , verbDefinitions = ["go"] }
      , defaultVerb
          { verbRoot = "muDi"
          , verbDefinitions = ["be finished", "be completed"]
          , verbInanimate = True }
      , defaultVerb
          { verbRoot = "uDai"
          , verbDefinitions = ["break" ~# "intransitive", "be broken"]
          , verbInanimate = True }
      , defaultVerb
          { verbRoot = "teri"
          , verbDefinitions = ["be known", "be visible", "be apparent"]
          , verbInanimate = True }
      , defaultVerb
          { verbRoot = "puri"
          , verbDefinitions = ["be understood"]
          , verbInanimate = True }
      , defaultVerb
          { verbRoot = "uTkaar"
          , verbDefinitions = ["sit"] } ]

    -- Class 2 Strong
  , setClass (Class2 Strong)
      [ defaultVerb
          { verbRoot = "nil"
          , verbDefinitions = ["stop", "stand"] }
      , defaultVerb
          { verbRoot = "naDa"
          , verbDefinitions = ["walk", "happen"] }
      , defaultVerb
          { verbRoot = "paRa"
          , verbDefinitions = ["fly"] }
      , defaultVerb
          { verbRoot = "maRa"
          , verbDefinitions = ["forget"] }
      , defaultVerb
          { verbRoot = "iru"
          , verbDefinitions = ["be", "stay"] } ]

    -- Class 3
  , setClass Class3
      [ defaultVerb
          { verbRoot = "aa"
          , verbDefinitions = ["become"] }
      , defaultVerb
          { verbRoot = "pO"
          , verbDefinitions = ["go", "leave"] }
      , defaultVerb
          { verbRoot = "sol"
          , verbDefinitions = ["say", "tell"] }
      , defaultVerb
          { verbRoot = "ODu"
          , verbDefinitions = ["run"] }
      , defaultVerb
          { verbRoot = "thoongu"
          , verbDefinitions = ["sleep"] }
      , defaultVerb
          { verbRoot = "tirumbu"
          , verbDefinitions = ["turn", "return"] }
      , defaultVerb
          { verbRoot = "paaDu"
          , verbDefinitions = ["sing"] }
      , defaultVerb
          { verbRoot = "pEsu"
          , verbDefinitions = ["speak", "talk"] }
      , defaultVerb
          { verbRoot = "veTTu"
          , verbDefinitions = ["cut"] }
      , defaultVerb
          { verbRoot = "vaangu"
          , verbDefinitions = ["buy"] } ] ]
  where
    setClass verbClass =
      map \verb -> verb { verbClass }

