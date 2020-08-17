module Conjugation
  ( IrrationalSubject (..)
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

data Subject
  = Naan
  | Naam
  | Naangal
  | Nee
  | Neengal
  | Avan
  | Aval
  | Avar
  | Avargal
  | Irrational IrrationalSubject
  deriving Eq

instance TamilShow Subject where
  tamilShow = \case
    Naan         -> "naan"
    Naam         -> "naam"
    Naangal      -> "naangaL"
    Nee          -> "nee"
    Neengal      -> "neengaL"
    Avan         -> "avan"
    Aval         -> "avaL"
    Avar         -> "avar"
    Avargal      -> "avargaL"
    Irrational i -> tamilShow i

instance Show Subject where
  show = map toLower . toLatin . tamilShow

allSubjects :: [Subject]
allSubjects =
  [ Naan, Naam, Naangal
  , Nee, Neengal
  , Avan, Aval, Avar, Avargal
  , Irrational Adhu, Irrational Avai ]

simpleSuffix :: Subject -> TamilString
simpleSuffix = \case
  Naan            -> "En"
  Naam            -> "Om"
  Naangal         -> "Om"
  Nee             -> "aay"
  Neengal         -> "eergaL"
  Avan            -> "aan"
  Aval            -> "aaL"
  Avar            -> "aar"
  Avargal         -> "aargaL"
  Irrational Adhu -> "adhu"
  Irrational Avai -> "ana"

subjectSuffix :: Subject -> ChoiceString
subjectSuffix = \case
  Avargal ->
    ChoiceString ["aargaL"] ["anar"] []
  other ->
    best $ simpleSuffix other

usingAvaiSuffix :: Subject -> (Bool -> ChoiceString) -> ChoiceString
usingAvaiSuffix subject f =
  case subject of
    Avargal ->
      (f False |+ "aargaL") <> (f True |+| common "ar")
    Irrational Avai ->
      f True |+ "a"
    other ->
      f False |+ simpleSuffix other

getPast :: Verb -> ChoiceString
getPast verb =
  case verbPast verb of
    Just past -> past
    Nothing ->
      let root = verbRoot verb in
      case (verbClass verb, getEnding root) of
        (Class1 Weak, HardAndU h) ->
          best $ replaceLastLetter root $ TamilString [Consonant $ Hard h, Consonant $ Hard h]
        (Class1 _, Retroflex) ->
          best $ replaceLastLetter root "TT"
        (Class1 _, Alveolar) ->
          best $ replaceLastLetter root "RR"
        (Class1 Weak, _) ->
          best $ suffix root "dh"
        (Class1 Strong, _) ->
          best $ suffix root "tt"
        (Class2 _, Retroflex) ->
          best $ replaceLastLetter root "NT"
        (Class2 _, Alveolar) ->
          best $ replaceLastLetter root "ndR"
        (Class2 _, _) ->
          best $ suffix root "ndh"
        (Class3, ending) ->
          let basic = suffix root "in" in
          case ending of
            LongVowel ->
              ChoiceString [root `append` "n"] [] [basic, root `append` "gin"]
            Retroflex ->
              ChoiceString [replaceLastLetter root "NN"] [] [basic]
            Alveolar ->
              ChoiceString [replaceLastLetter root "nn"] [] [basic]
            _ ->
              best basic

addToStem :: Verb -> Consonant -> Vallinam -> ChoiceString
addToStem verb ifWeak ifStrong =
  forChoice (getStem verb) \stem ->
    case getStrength verb of
      Weak ->
        -- If weak, use the weak letter
        stem `appendLetter` Consonant ifWeak
      Strong ->
        -- If strong, use the strong letter
        let
          strongLetter = Consonant $ Hard ifStrong
          withSingle = stem `appendLetter` strongLetter
        in
          if endsInHardConsonant stem then
            -- If the stem ends in a hard consonant, then leave it single
            withSingle
          else
            -- Otherwise, double it
            withSingle `appendLetter` strongLetter

getPresent :: Verb -> Bool -> ChoiceString
getPresent verb avai =
  if avai then
    root |+ "indRan"
  else
    root |+| ChoiceString ["iR"] [] ["indR"]
  where
    root =
      forChoice (getStem verb) \stem ->
        stem `append` case getStrength verb of
          Strong | not $ endsInHardConsonant stem -> "kk"
          _ -> "k"

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

getFutureAdhu :: Verb -> TamilString
getFutureAdhu verb =
  let stem = collapse $ getStem verb in
  stem `append`
    if endsInHardConsonant stem || endsInLongVowel stem then
      "k"
    else
      case getStrength verb of
        Weak   -> ""
        Strong -> "kk"

getNoun :: Verb -> TamilString
getNoun verb =
  suffix (collapse $ getFuture verb) "adhu"

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
            ChoiceString [gi] [] [y, yi]
          else
            ChoiceString [y] [] [yi, gi]
      else
        best $ suffix root "i"
    _ ->
      getPast verb |+ "u"

getInfinitive :: Verb -> TamilString
getInfinitive verb =
  suffix (getFutureAdhu verb) "a"

getStem :: Verb -> ChoiceString
getStem verb =
  case verbStem verb of
    Just stem -> stem
    Nothing ->
      case getStrength verb of
        Weak ->
          getRoot verb
        Strong ->
          let root = verbRoot verb in
          case getEnding root of
            Retroflex ->
              best $ replaceLastLetter root "T"
            Alveolar ->
              best $ replaceLastLetter root "R"
            _ ->
              best root

data FiniteConjugation
  = Past
  | Present
  | Future
  deriving Show

conjugateFinite :: FiniteConjugation -> Subject -> Verb -> ChoiceString
conjugateFinite conjugation subject verb =
  case (conjugation, verbClass verb, subject) of
    (Past, Class3, Irrational Adhu) ->
      let
        past = getPast verb
        nadhu = past |+ "adhu"
        yadhu =
          filterMapChoice past \case
            TamilString (Consonant (Soft NAlveolar) : rest@(Vowel _ : _)) ->
              Just (TamilString rest `append` "yadhu")
            _ ->
              Nothing
        yitru =
          filterMapChoice past \case
            TamilString (Consonant (Soft NAlveolar) : rest@(Vowel (I Short) : _)) ->
              Just (TamilString rest `append` "tRu")
            _ ->
              Nothing
      in
        if endsInLongVowel $ verbRoot verb then
          promote nadhu <> halfPromote yadhu <> halfPromote yitru
        else
          promote yadhu <> halfPromote nadhu <> halfPromote yitru
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
    (Future, _, Irrational _) ->
      best $ suffix (getFutureAdhu verb) "um"
    (Future, _, _) ->
      getFuture verb |+ simpleSuffix subject

type Respectful = Bool

data PositiveConjugation
  = Finite FiniteConjugation Subject
  | Adjective FiniteConjugation
  | Noun
  | Adverb
  | Command Respectful
  deriving Show

conjugatePositive :: PositiveConjugation -> Verb -> ChoiceString
conjugatePositive conjugation verb =
  case conjugation of
    Finite finite subject ->
      conjugateFinite finite subject verb
    Adjective finite ->
      forChoice (conjugateFinite finite (Irrational Adhu) verb) \case
        TamilString (Vowel (U Short) : Consonant (Hard TDental) : rest) ->
          TamilString rest
        other ->
          other
    Adverb ->
      getAdverb verb
    Noun ->
      best $ getNoun verb
    Command False ->
      getRoot verb
    Command True ->
      case verbRespectfulCommand verb of
        Just command -> command
        Nothing ->
          let root = verbRoot verb in
          case verbClass verb of
            Class3 | endsInLongVowel root ->
              let
                basic = root `append` "ngaL"
                alt = root `append` "gungaL"
              in
                if isSingleLetter root then
                  ChoiceString [alt] [] [basic]
                else
                  ChoiceString [basic] [] [alt]
            _ ->
              best $ suffix root "ungaL"

data NegativeConjugation
  = NegativePastPresent
  | NegativeFuture Subject
  | NegativeHabitual
  | NegativeAdjective
  | NegativeNoun
  | NegativeAdverb
  | NegativeCommand Respectful
  deriving Show

conjugateNegative :: NegativeConjugation -> Verb -> ChoiceString
conjugateNegative conjugation verb =
  case conjugation of
    NegativePastPresent ->
      best $ suffix (getInfinitive verb) "illai"
    NegativeFuture (Irrational subject) ->
      let
        futureAdhu = getFutureAdhu verb
        aadhu = suffix futureAdhu "aadhu"
      in
        case subject of
          Adhu ->
            best aadhu
          Avai ->
            ChoiceString [aadhu] [] [suffix futureAdhu "aa"]
    NegativeFuture subject ->
      best $ suffix (suffix (getInfinitive verb) "maaTT") $ simpleSuffix subject
    NegativeHabitual ->
      best $ suffix (getNoun verb) "illai"
    NegativeAdjective ->
      let futureAdhu = getFutureAdhu verb in
      ChoiceString [suffix futureAdhu "aadha"] [suffix futureAdhu "aa"] []
    NegativeNoun ->
      best $ suffix (getFutureAdhu verb) "aadhadhu"
    NegativeAdverb ->
      best $ suffix (getFutureAdhu verb) "aamal"
    NegativeCommand False ->
      best $ suffix (getFutureAdhu verb) "aadhE"
    NegativeCommand True ->
      best $ suffix (getFutureAdhu verb) "aadheergaL"

data Conjugation
  = Positive PositiveConjugation
  | Negative NegativeConjugation
  | Infinitive
  deriving Show

conjugate :: Conjugation -> Verb -> ChoiceString
conjugate conjugation verb =
  verbPrefix verb |+|
    case conjugation of
      Positive positive ->
        conjugatePositive positive verb
      Negative negative ->
        conjugateNegative negative verb
      Infinitive ->
        best $ getInfinitive verb
