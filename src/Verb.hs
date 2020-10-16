module Verb where

import TamilString

import Control.Monad

import Data.List
import Data.Char
import Data.Hashable

import Data.Set (Set)
import qualified Data.Set as Set
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap

data NorL = N | L

data VerbEnding
  = HardAndU Vallinam
  | LongVowel
  | Retroflex NorL
  | Alveolar NorL
  | RLike
  | OtherEnding

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

data Strength
  = Weak
  | Strong
  deriving (Ord, Eq)

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

-- CLASS [PREFIX] ROOT. (DEFINITION),+ (. FLAG)*
-- 2W koNDu vaa. bring. adv vandhu. stem varu. resp vaarungaL

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

parseTamilNonEmpty :: String -> Either String TamilString
parseTamilNonEmpty s =
  case parseTamil s of
    Right (TamilString []) ->
      Left "word cannot be empty"
    other -> other

parseChoiceString :: String -> Either String ChoiceString
parseChoiceString s =
  case map (split ',') $ split ';' s of
    [c] ->
      (`ChoiceString` []) <$> getTamil c
    [c, o] ->
      ChoiceString <$> getTamil c <*> getTamil o
    _ ->
      Left "expected either 1 or 2 sets of choices"
  where
    getTamil = traverse parseTamilNonEmpty

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
          verbDefinitions ->
            if any (any \x -> not (isAlpha x) && x /= ' ') verbDefinitions then
              Left "verb definitions cannot contain special characters, only [a-z]"
            else
              Right verb { verbDefinitions }
      foldM addFlag verb $ map words flags
    _ ->
      Left "missing definition for verb"
  where
    checkValid kind unparsed str =
      case validateTamil str of
        Left err ->
          Left $ "invalid " ++ kind ++ " '" ++ unparsed ++ "': " ++ err
        other -> other
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
      | otherwise          = Right verb { verbDefective = True }
    addFlag _ [flag] =
      Left $ "invalid flag for verb: " ++ flag
    addFlag verb (key : strParts) = do
      choiceStr@(ChoiceString c u) <- parseChoiceString $ concat strParts
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

parseAllVerbs :: String -> ([String], VerbList)
parseAllVerbs file =
  go (lines file) (1 :: Int) [] []
  where
    go lines n errs verbs =
      case lines of
        [] ->
          (reverse errs, makeVerbList verbs)
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
                  go rest n' errs (verb:verbs)
                Left e ->
                  let err = "syntax error in line " ++ show n ++ ": " ++ e in
                  go rest n' (err:errs) verbs

data Verb = Verb
  { verbRoot :: TamilString
  , verbPrefix :: TamilString
  , verbDefinitions :: [String]
  , verbDefective :: Bool
  , verbAdverb :: Maybe ChoiceString
  , verbStem :: Maybe ChoiceString
  , verbFutureAdhu :: Maybe ChoiceString
  , verbInfinitiveRoot :: Maybe ChoiceString
  , verbRespectfulCommandRoot :: Maybe ChoiceString
  , verbClass :: VerbClass }

instance Eq Verb where
  a == b =
    verbClass a == verbClass b
    && verbRoot a == verbRoot b

instance Ord Verb where
  a `compare` b =
    verbClass a `compare` verbClass b
    <> verbRoot a `compare` verbRoot b
    <> verbPrefix a `compare` verbPrefix b

instance Show Verb where
  show Verb { .. } =
    show verbClass ++ " " ++ maybePrefix ++ rootStr ++ ". " ++ definitions ++ flags
    where
      definitions =
        case verbDefinitions of
          [] -> "???"
          _  -> intercalate ", " verbDefinitions
      (maybePrefix, rootStr) =
        case verbPrefix of
          TamilString [] ->
            ("", toLatin verbRoot)
          _ ->
            (,) (toLatin verbPrefix ++ " ")
              case toLatin verbRoot of
                ('s':rest) | TamilString (Consonant (Hard _) : _) <- verbPrefix ->
                  "ch" ++ rest
                root ->
                  root
      addFlag _ False s = s
      addFlag f True s = ". " ++ f ++ s
      addKey _ Nothing s = s
      addKey k (Just c) s = ". " ++ k ++ " " ++ showUsing toLatin c ++ s
      flags =
        addFlag "defect" verbDefective $
        addKey "adv" verbAdverb $
        addKey "stem" verbStem $
        addKey "adhu" verbFutureAdhu $
        addKey "inf" ((|+ "a") <$> verbInfinitiveRoot) $
        addKey "resp" ((|+ "ngaL") <$> verbRespectfulCommandRoot) $
        ""

defaultVerb :: Verb
defaultVerb = Verb
  { verbRoot = undefined
  , verbPrefix = ""
  , verbDefinitions = []
  , verbDefective = False
  , verbAdverb = Nothing
  , verbStem = Nothing
  , verbFutureAdhu = Nothing
  , verbInfinitiveRoot = Nothing
  , verbRespectfulCommandRoot = Nothing
  , verbClass = undefined }

getRoot :: Verb -> ChoiceString
getRoot verb =
  let root = verbRoot verb in
  case (verbClass verb, getEnding root) of
    (Class2 Weak, RLike) ->
      ChoiceString [root] [root `append` "u"]
    (Class3, LongVowel) ->
      ChoiceString [root] [root `append` "gu"]
    _ ->
      if endsInDoublingConsonant root then
        ChoiceString [root] [suffix root "u"]
      else
        common root

getStrength :: Verb -> Strength
getStrength verb =
  case verbClass verb of
    Class1 s -> s
    Class2 s -> s
    Class3   -> Weak

data VerbList = VerbList
  { allVerbs :: Set Verb
  , byRoot :: !(HashMap TamilString [Verb])
  , byDefinition :: !(HashMap String [Verb]) }

emptyVerbList :: VerbList
emptyVerbList = VerbList
  { allVerbs = Set.empty
  , byRoot = HashMap.empty
  , byDefinition = HashMap.empty }

addVerb :: Verb -> VerbList -> VerbList
addVerb basicVerb verbList@VerbList { allVerbs, byRoot, byDefinition }
  | v `Set.member` allVerbs = verbList
  | otherwise = VerbList
    { allVerbs = Set.insert v allVerbs
    , byRoot = withAllNames
    , byDefinition = foldr insertVerb byDefinition $ verbDefinitions v }
  where
    v = basicVerb { verbDefinitions = map stripTo $ verbDefinitions basicVerb }

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

    insertVerb :: (Eq k, Hashable k) => k -> HashMap k [Verb] -> HashMap k [Verb]
    insertVerb = HashMap.alter \case
      Nothing -> Just [v]
      Just vs -> Just $ v : vs

stripTo :: String -> String
stripTo (' ' : s) = stripTo s
stripTo ('t' : 'o' : ' ' : s) = stripTo s
stripTo other = other

makeVerbList :: [Verb] -> VerbList
makeVerbList = foldl' (flip addVerb) emptyVerbList

defaultVerbList :: VerbList
defaultVerbList = makeVerbList
  [ -- Class 1 Weak
    defaultVerb
      { verbRoot = "saappiDu"
      , verbDefinitions = ["eat"]
      , verbClass = Class1 Weak }
  , defaultVerb
      { verbRoot = "paDu"
      , verbDefinitions = ["experience", "undergo", "suffer"]
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
      , verbDefinitions = ["lie down", "go to bed"]
      , verbClass = Class1 Strong }
  , defaultVerb
      { verbRoot = "paar"
      , verbDefinitions = ["look", "see"]
      , verbClass = Class1 Strong }
  , defaultVerb
      { verbRoot = "muDi"
      , verbDefinitions = ["finish", "complete"]
      , verbClass = Class1 Strong }
  , defaultVerb
      { verbRoot = "uDai"
      , verbDefinitions = ["break"]
      , verbClass = Class1 Strong }
    -- Class 2 Weak
  , defaultVerb
      { verbRoot = "thaa"
      , verbDefinitions = ["give"]
      , verbAdverb = Just "thandhu"
      , verbStem = Just "tharu"
      , verbRespectfulCommandRoot = Just "thaaru"
      , verbClass = Class2 Weak }
  , defaultVerb
      { verbRoot = "vaa"
      , verbDefinitions = ["come"]
      , verbAdverb = Just "vandhu"
      , verbStem = Just "varu"
      , verbRespectfulCommandRoot = Just "vaaru"
      , verbClass = Class2 Weak }
  , defaultVerb
      { verbRoot = "koL"
      , verbDefinitions = ["have", "hold"]
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
      , verbDefinitions = ["be finished", "be completed"]
      , verbDefective = True
      , verbClass = Class2 Weak }
  , defaultVerb
      { verbRoot = "uDai"
      , verbDefinitions = ["be broken"]
      , verbDefective = True
      , verbClass = Class2 Weak }
  , defaultVerb
      { verbRoot = "teri"
      , verbDefinitions = ["be known", "be visible", "be apparent"]
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
      { verbRoot = "aa"
      , verbDefinitions = ["become", "happen", "be done"]
      , verbClass = Class3 }
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

