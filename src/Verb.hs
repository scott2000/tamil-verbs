module Verb where

import TamilString

import Data.Hashable

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap

data VerbEnding
  = HardAndU Vallinam
  | LongVowel
  | Retroflex
  | Alveolar
  | RLike
  | OtherEnding

getEnding :: TamilString -> VerbEnding
getEnding (TamilString str) =
  case str of
    Consonant c : Vowel _ : _ ->
      case c of
        Soft   NRetroflex -> Retroflex
        Soft   NAlveolar  -> Alveolar
        Medium LRetroflex -> Retroflex
        Medium LAlveolar  -> Alveolar
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

data Verb = Verb
  { verbRoot :: TamilString
  , verbPrefix :: ChoiceString
  , verbDefinitions :: [String]
  , verbDefective :: Bool
  , verbPast :: Maybe ChoiceString
  , verbStem :: Maybe ChoiceString
  , verbFuture :: Maybe ChoiceString
  , verbFutureAdhu :: Maybe ChoiceString
  , verbInfinitiveRoot :: Maybe ChoiceString
  , verbRespectfulCommand :: Maybe ChoiceString
  , verbClass :: VerbClass }

instance Eq Verb where
  a == b =
    verbClass a == verbClass b
    && verbRoot a == verbRoot b

instance Ord Verb where
  a `compare` b =
    verbClass a `compare` verbClass b
    <> verbRoot a `compare` verbRoot b

defaultVerb :: Verb
defaultVerb = Verb
  { verbRoot = undefined
  , verbPrefix = ""
  , verbDefinitions = []
  , verbDefective = False
  , verbPast = Nothing
  , verbStem = Nothing
  , verbFuture = Nothing
  , verbFutureAdhu = Nothing
  , verbInfinitiveRoot = Nothing
  , verbRespectfulCommand = Nothing
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
  { allVerbs :: [Verb]
  , byRoot :: !(HashMap TamilString [Verb])
  , byDefinition :: !(HashMap String [Verb]) }

emptyVerbList :: VerbList
emptyVerbList = VerbList
  { allVerbs = []
  , byRoot = HashMap.empty
  , byDefinition = HashMap.empty }

addVerb :: Verb -> VerbList -> VerbList
addVerb basicVerb VerbList { allVerbs, byRoot, byDefinition } = VerbList
  { allVerbs = v : allVerbs
  , byRoot = insertAll (verbPrefix v |+| getRoot v) byRoot
  , byDefinition = foldr insertVerb byDefinition $ verbDefinitions v }
  where
    v = basicVerb { verbDefinitions = map stripTo $ verbDefinitions basicVerb }

    insertAll cs m =
      foldr insertVerb m $ allChoices cs

    insertVerb :: (Eq k, Hashable k) => k -> HashMap k [Verb] -> HashMap k [Verb]
    insertVerb = HashMap.alter \case
      Nothing -> Just [v]
      Just vs -> Just $ v : vs

stripTo :: String -> String
stripTo (' ' : s) = stripTo s
stripTo ('t' : 'o' : ' ' : s) = stripTo s
stripTo other = other

