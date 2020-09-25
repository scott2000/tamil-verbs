module Conjugation
  ( IrrationalSubject (..)
  , allIrrationalSubjects
  , ThirdPersonSubject (..)
  , allThirdPersonSubjects
  , Subject (..)
  , allSubjects
  , FiniteConjugation (..)
  , PositiveConjugation (..)
  , NegativeConjugation (..)
  , Conjugation (..)
  , conjugate
  ) where

import TamilString
import Verb

import Data.Char

data IrrationalSubject
  = Adhu
  | Avai
  deriving Eq

instance TamilShow IrrationalSubject where
  tamilShow = \case
    Adhu    -> "adhu"
    Avai    -> "avai"

instance Show IrrationalSubject where
  show = map toLower . toLatin . tamilShow

allIrrationalSubjects :: [IrrationalSubject]
allIrrationalSubjects = [Adhu, Avai]

data ThirdPersonSubject
  = Avan
  | Aval
  | Avar
  | Avargal
  | Irrational IrrationalSubject
  deriving Eq

allThirdPersonSubjects :: [ThirdPersonSubject]
allThirdPersonSubjects =
  [ Avan, Aval, Avar, Avargal ]
  ++ map Irrational allIrrationalSubjects

vRelativeSuffix :: ThirdPersonSubject -> ChoiceString
vRelativeSuffix = \case
  Avan            -> "On"
  Avar            -> "Or"
  Irrational Adhu -> "adhu"
  _               -> mempty

pRelativeSuffix :: ThirdPersonSubject -> ChoiceString
pRelativeSuffix = \case
  Irrational Adhu ->
    mempty
  other ->
    common $ tamilShow other

relativeSuffix :: ThirdPersonSubject -> ChoiceString
relativeSuffix subject =
  promote $ pRelativeSuffix subject <> demote (vRelativeSuffix subject)

instance TamilShow ThirdPersonSubject where
  tamilShow = \case
    Avan         -> "avan"
    Aval         -> "avaL"
    Avar         -> "avar"
    Avargal      -> "avargaL"
    Irrational i -> tamilShow i

instance Show ThirdPersonSubject where
  show = map toLower . toLatin . tamilShow

data Subject
  = Naan
  | Naam
  | Naangal
  | Nee
  | Neer
  | Neengal
  | Third ThirdPersonSubject
  deriving Eq

instance TamilShow Subject where
  tamilShow = \case
    Naan    -> "naan"
    Naam    -> "naam"
    Naangal -> "naangaL"
    Nee     -> "nee"
    Neer    -> "neer"
    Neengal -> "neengaL"
    Third t -> tamilShow t

instance Show Subject where
  show = map toLower . toLatin . tamilShow

allSubjects :: [Subject]
allSubjects =
  [ Naan, Naam, Naangal
  , Nee, Neer, Neengal ]
  ++ map Third allThirdPersonSubjects

simpleSuffix :: Subject -> TamilString
simpleSuffix = \case
  Naan                    -> "En"
  Naam                    -> "Om"
  Naangal                 -> "Om"
  Nee                     -> "aay"
  Neer                    -> "eer"
  Neengal                 -> "eergaL"
  Third Avan              -> "aan"
  Third Aval              -> "aaL"
  Third Avar              -> "aar"
  Third Avargal           -> "aargaL"
  Third (Irrational Adhu) -> "adhu"
  Third (Irrational Avai) -> "ana"

subjectSuffix :: Subject -> ChoiceString
subjectSuffix = \case
  Third Avargal ->
    ChoiceString ["aargaL", "anar"] []
  other ->
    common $ simpleSuffix other

usingAvaiSuffix :: Subject -> (Bool -> ChoiceString) -> ChoiceString
usingAvaiSuffix subject f =
  case subject of
    Third Avargal ->
      (f False |+ "aargaL") <> (f True |+ "ar")
    Third (Irrational Avai) ->
      f True |+ "a"
    other ->
      f False |+ simpleSuffix other

getPast :: Verb -> ChoiceString
getPast verb =
  case verbAdverb verb of
    Just adverb -> adverb
    Nothing ->
      let root = verbRoot verb in
      case (verbClass verb, getEnding root) of
        (Class1 Weak, HardAndU h) ->
          common $ replaceLastLetter root $ TamilString [Consonant $ Hard h, Consonant $ Hard h]
        (Class1 _, Retroflex L) ->
          common $ replaceLastLetter root "TT"
        (Class1 _, Alveolar L) ->
          common $ replaceLastLetter root "RR"
        (Class1 Weak, _) ->
          common $ suffix root "dh"
        (Class1 Strong, _) ->
          common $ suffix root "tt"
        (Class2 _, Retroflex _) ->
          common $ replaceLastLetter root "NT"
        (Class2 _, Alveolar _) ->
          common $ replaceLastLetter root "ndR"
        (Class2 _, _) ->
          common $ suffix root "ndh"
        (Class3, LongVowel) ->
          ChoiceString [root `append` "n"] [suffix root "in", root `append` "gin"]
        (Class3, Alveolar L) | isShortish root ->
          ChoiceString [replaceLastLetter root "nn"] [suffix root "in"]
        (Class3, _) ->
          common $ suffix root "in"

oneOrTwoOnStem :: Vallinam -> Verb -> ChoiceString
oneOrTwoOnStem hard verb =
  forChoice (getStem verb) \stem ->
    stem `append` case getStrength verb of
      Strong | not $ endsInHardConsonant stem ->
        TamilString [c, c]
      _ ->
        TamilString [c]
  where
    c = Consonant $ Hard hard

getPresent :: Verb -> Bool -> ChoiceString
getPresent verb avai =
  if avai then
    oneOrTwoOnStem K verb |+ "indRan"
  else
    oneOrTwoOnStem K verb |+| ChoiceString ["iR"] ["indR"]

{-

Special Rules:

Vallinam or long vowel ending => -க infinitive
Vallinam or mellinam ending   => -ப- future

Class 3 with long vowel ending => -ன் - past, -ய் adverb, -ங்கள் command

-}

getFuture :: Verb -> ChoiceString
getFuture verb =
  forChoice (getStem verb) \stem ->
    stem `append`
      if endsInHardConsonant stem || endsInSoftConsonant stem then
        "p"
      else
        case getStrength verb of
          Weak   -> "v"
          Strong -> "pp"

getFutureAdhu :: Verb -> ChoiceString
getFutureAdhu verb =
  case verbFutureAdhu verb of
    Just future -> future
    Nothing ->
      let stem = collapse $ getStem verb in
      case getStrength verb of
        Weak ->
          if endsInLongVowel stem then
            let basic = stem `append` "gum" in
            case verbClass verb of
              Class3 ->
                let alt = stem `append` "m" in
                if isSingleLetter stem then
                  ChoiceString [alt, basic] []
                else
                  ChoiceString [basic] [alt]
              _ ->
                common basic
          else
            common $ suffix stem "um"
        Strong ->
          common $ stem `append`
            if endsInHardConsonant stem then
              "kum"
            else
              "kkum"

getInfinitiveRootKind :: StemKind -> Verb -> ChoiceString
getInfinitiveRootKind kind verb =
  case verbInfinitiveRoot verb of
    Just root -> root
    Nothing ->
      let stem = collapse $ getStemKind kind verb in
      case getStrength verb of
        Weak ->
          if endsInLongVowel stem then
            let basic = stem `append` "g" in
            case verbClass verb of
              Class3 ->
                common basic
              _ ->
                ChoiceString [basic] [stem]
          else
            common stem
        Strong ->
          common $
            case getEnding stem of
              Retroflex L -> stem
              Alveolar L -> stem
              _ ->
                stem `append`
                  if endsInHardConsonant stem then
                    "k"
                  else
                    "kk"

getInfinitiveRoot :: Verb -> ChoiceString
getInfinitiveRoot verb =
  let plain = getInfinitiveRootKind StemPlain verb in
  case getStrength verb of
    Weak -> plain
    Strong ->
      getInfinitiveRootKind StemStrengthBased verb <> demote plain

getInfinitive :: Verb -> ChoiceString
getInfinitive verb =
  getInfinitiveRootKind StemStrengthBased verb |+ "a"

getNegativeFutureAdhuRoot :: Verb -> ChoiceString
getNegativeFutureAdhuRoot verb =
  getInfinitiveRoot verb <> demote (getRespectfulCommandRoot verb)

getNegativeFutureAvai :: Verb -> ChoiceString
getNegativeFutureAvai verb =
  getNegativeFutureAdhuRoot verb |+| ("aadhu" <> uncommon "aa")

getNounAdhu :: Verb -> ChoiceString
getNounAdhu = conjugateRelative Future $ Irrational Adhu

getNounThal :: Verb -> ChoiceString
getNounThal verb =
  let stem = collapse $ getStem verb in
  case getStrength verb of
    Weak ->
      if endsInLongVowel stem then
        let basic = stem `append` "gudhal" in
        case verbClass verb of
          Class3 ->
            ChoiceString [basic] [stem `append` "dhal"]
          _ ->
            common basic
      else
        if endsInDoublingConsonant stem then
          common $ suffix stem "udhal"
        else
          common $ stem `append` "dhal"
    Strong ->
      if endsInHardConsonant stem then
        common $ stem `append` "kudhal"
      else
        ChoiceString [stem `append` "ttal"] [stem `append` "kkudhal"]

getNounKai :: Verb -> ChoiceString
getNounKai verb =
  oneOrTwoOnStem K verb |+ "ai"

getNounAl :: Verb -> ChoiceString
getNounAl verb =
  getInfinitive verb |+ "l"

getAdverb :: Verb -> ChoiceString
getAdverb verb =
  case verbClass verb of
    Class3 ->
      let root = verbRoot verb in
      if endsInLongVowel root then
        let
          y = root `append` "y"
          gi = root `append` "gi"
          yi = root `append` "yi"
        in
          if isSingleLetter root then
            ChoiceString [gi, y] [yi]
          else
            ChoiceString [y] [yi, gi]
      else
        common $ suffix root "i"
    _ ->
      getPast verb |+ "u"

getRespectfulCommandRoot :: Verb -> ChoiceString
getRespectfulCommandRoot verb =
  case verbRespectfulCommandRoot verb of
    Just command -> command
    Nothing ->
      let root = verbRoot verb in
      case verbClass verb of
        Class3 | endsInLongVowel root ->
          let alt = root `append` "gu" in
          if isSingleLetter root then
            ChoiceString [alt] [root]
          else
            ChoiceString [root] [alt]
        _ ->
          common $ suffix root "u"

data StemKind
  = StemStrengthBased
  | StemPlain

getStemKind :: StemKind -> Verb -> ChoiceString
getStemKind kind verb =
  case kind of
    StemStrengthBased ->
      case strength of
        Weak -> plain
        Strong ->
          common sub
    StemPlain -> plain
  where
    strength = getStrength verb
    plain =
      case verbStem verb of
        Just stem -> stem
        Nothing -> getRoot verb
    sub =
      let stem = collapse plain in
      case getEnding stem of
        Retroflex L ->
          replaceLastLetter stem "T"
        Alveolar L ->
          replaceLastLetter stem "R"
        _ ->
          stem

getStem :: Verb -> ChoiceString
getStem = getStemKind StemStrengthBased

data FiniteConjugation
  = Past
  | Present
  | Future
  deriving Show

getClass3PastNYForm :: Verb -> ChoiceString
getClass3PastNYForm verb =
  let
    past = getPast verb
    withY =
      filterMapChoice past \case
        TamilString (Consonant (Soft NAlveolar) : rest@(Vowel _ : _)) ->
          Just $ TamilString (Consonant (Medium Y) : rest)
        _ ->
          Nothing
    isIrregular =
      case getEnding $ verbRoot verb of
        LongVowel  -> True
        Alveolar L -> True
        _          -> False
  in
    if isIrregular then
      promote past <> promote withY
    else
      promote withY <> promote past

conjugateFinite :: FiniteConjugation -> Subject -> Verb -> ChoiceString
conjugateFinite conjugation subject verb =
  case (conjugation, verbClass verb, subject) of
    (Past, Class3, Third (Irrational Adhu)) ->
      let
        ny = getClass3PastNYForm verb
        yitru =
          filterMapChoice ny \case
            TamilString (Consonant (Soft NAlveolar) : rest@(Vowel (I Short) : _)) ->
              Just (TamilString rest `append` "tRu")
            _ ->
              Nothing
      in
        ny |+ "adhu" <> promote yitru
    (Past, _, _) ->
      usingAvaiSuffix subject \avai ->
        if avai then
          if verbClass verb == Class3 then
            promote $ filterMapChoice (getAdverb verb) \case
              str@(TamilString (Vowel (I Short) : _)) ->
                Just (str `append` "n")
              _ ->
                Nothing
          else
            getPast verb |+ "an"
        else
          getPast verb
    (Present, _, _) ->
      usingAvaiSuffix subject $ getPresent verb
    (Future, _, Third (Irrational _)) ->
      getFutureAdhu verb
    (Future, _, _) ->
      getFuture verb |+ simpleSuffix subject

conjugateRelative :: FiniteConjugation -> ThirdPersonSubject -> Verb -> ChoiceString
conjugateRelative conjugation subject verb =
  case conjugation of
    Past
      | verbClass verb == Class3 ->
        getClass3PastNYForm verb |+| subjectSuffix
      | otherwise ->
        getPast verb |+| subjectSuffix
    Present ->
      getPresent verb False |+| subjectSuffix
    Future ->
      promote $
        oneOrTwoOnStem P verb |+| pRelativeSuffix subject
        <> getFuture verb |+| demote (vRelativeSuffix subject)
  where
    subjectSuffix = relativeSuffix subject

type Respectful = Bool

data PositiveConjugation
  = Finite FiniteConjugation Subject
  | Adjective FiniteConjugation
  | Relative FiniteConjugation ThirdPersonSubject
  | Noun
  | Adverb
  | Conditional
  | Command Respectful
  deriving Show

conjugatePositive :: PositiveConjugation -> Verb -> ChoiceString
conjugatePositive conjugation verb =
  case conjugation of
    Finite finite subject ->
      conjugateFinite finite subject verb
    Adjective finite ->
      forChoice (conjugateFinite finite (Third (Irrational Adhu)) verb) \case
        TamilString (Vowel (U Short) : Consonant (Hard TDental) : rest) ->
          TamilString rest
        other ->
          other
    Relative finite subject ->
      conjugateRelative finite subject verb
    Adverb ->
      getAdverb verb
    Noun ->
      getNounAdhu verb <> getNounThal verb <> getNounKai verb <> getNounAl verb
    Conditional ->
      getPast verb |+ "aal"
    Command False ->
      getRoot verb
    Command True ->
      let commandRoot = getRespectfulCommandRoot verb in
      commandRoot |+ "ngaL" <> demote (commandRoot |+ "m")

data NegativeConjugation
  = NegativePastPresent
  | NegativeFuture Subject
  | NegativeHabitual
  | NegativeAdjective
  | NegativeRelative ThirdPersonSubject
  | NegativeNoun
  | NegativeAdverb
  | NegativeConditional
  | NegativeCommand Respectful
  deriving Show

conjugateNegative :: NegativeConjugation -> Verb -> ChoiceString
conjugateNegative conjugation verb =
  case conjugation of
    NegativePastPresent ->
      getInfinitive verb |+ "illai"
    NegativeFuture (Third (Irrational Adhu)) ->
      getNegativeFutureAdhuRoot verb |+ "aadhu"
    NegativeFuture (Third (Irrational Avai)) ->
      getNegativeFutureAvai verb
    NegativeFuture subject ->
      getInfinitive verb |+ suffix "maaTT" (simpleSuffix subject)
    NegativeHabitual ->
      getNounAdhu verb |+ "illai"
    NegativeAdjective ->
      getNegativeFutureAdhuRoot verb |+| ("aadha" <> "aa")
    NegativeRelative subject ->
      getNegativeFutureAdhuRoot verb |+ "aadhu" |+| relativeSuffix subject
    NegativeNoun ->
      getNegativeFutureAdhuRoot verb |+| ("aadhadhu" <> "aamai")
    NegativeAdverb ->
      getInfinitiveRoot verb |+ "aamal" <> demote (getNegativeFutureAvai verb)
    NegativeConditional ->
      getNegativeFutureAdhuRoot verb |+ "aaviTTaal"
    NegativeCommand False ->
      getNegativeFutureAdhuRoot verb |+ "aadhE"
    NegativeCommand True ->
      getNegativeFutureAdhuRoot verb |+ "aadheergaL"

data Conjugation
  = Positive PositiveConjugation
  | Negative NegativeConjugation
  | Infinitive
  deriving Show

conjugate :: Conjugation -> Verb -> ChoiceString
conjugate conjugation verb =
  common (verbPrefix verb) |+|
    case conjugation of
      Positive positive ->
        conjugatePositive positive verb
      Negative negative ->
        conjugateNegative negative verb
      Infinitive ->
        getInfinitive verb

