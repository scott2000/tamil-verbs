-- | Functions and data types for conjugating Tamil verbs
module Conjugation
  ( IrrationalSubject (..)
  , allIrrationalSubjects
  , ThirdPersonSubject (..)
  , allThirdPersonSubjects
  , Subject (..)
  , allSubjects
  , makeRespectful
  , TenseConjugation (..)
  , Respectful
  , PositiveConjugation (..)
  , NegativeConjugation (..)
  , Conjugation (..)
  , conjugate
  ) where

import TamilString
import Verb

import Data.Char

-- | Represents an irrational third person subject pronoun
data IrrationalSubject
  = Adhu
  | Avai
  deriving Eq

instance TamilShow IrrationalSubject where
  tamilShow = \case
    Adhu -> convertInitialN "adhu"
    Avai -> convertInitialN "avai"

instance Show IrrationalSubject where
  show = map toLower . toLatin . tamilShow

-- | A list of all possible 'IrrationalSubject's
allIrrationalSubjects :: [IrrationalSubject]
allIrrationalSubjects = [Adhu, Avai]

-- | Represents a third person subject pronoun
data ThirdPersonSubject
  = Avan
  | Aval
  | Avar
  | Avargal
  | Irrational IrrationalSubject
  deriving Eq

-- | A list of all possible 'ThirdPersonSubject's
allThirdPersonSubjects :: [ThirdPersonSubject]
allThirdPersonSubjects =
  [ Avan, Aval, Avar, Avargal ]
  ++ map Irrational allIrrationalSubjects

-- | Relative suffix for third person subjects which use V in the future
vRelativeSuffix :: ThirdPersonSubject -> ChoiceString
vRelativeSuffix = \case
  Avan            -> "On"
  Aval            -> "OL"
  Avar            -> "Or"
  Avargal         -> "OrgaL"
  Irrational Adhu -> "adhu"
  _               -> mempty

-- | Relative suffix for third person subjects which use P in the future
pRelativeSuffix :: ThirdPersonSubject -> ChoiceString
pRelativeSuffix = \case
  Irrational Adhu ->
    mempty
  other ->
    common $ tamilShow other

-- | Relative suffix for third person subjects for non-future tenses
relativeSuffix :: ThirdPersonSubject -> ChoiceString
relativeSuffix subject =
  promote $ pRelativeSuffix subject <> demote (vRelativeSuffix subject)

instance TamilShow ThirdPersonSubject where
  tamilShow = \case
    Avan         -> convertInitialN "avan"
    Aval         -> convertInitialN "avaL"
    Avar         -> convertInitialN "avar"
    Avargal      -> convertInitialN "avargaL"
    Irrational i -> tamilShow i

instance Show ThirdPersonSubject where
  show = map toLower . toLatin . tamilShow

-- | Represents a subject
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
    Naan    -> convertInitialN "naan"
    Naam    -> convertInitialN "naam"
    Naangal -> convertInitialN "naangaL"
    Nee     -> convertInitialN "nee"
    Neer    -> convertInitialN "neer"
    Neengal -> convertInitialN "neengaL"
    Third t -> tamilShow t

instance Show Subject where
  show = map toLower . toLatin . tamilShow

-- | A list of all possible 'Subject's
allSubjects :: [Subject]
allSubjects =
  [ Naan, Naam, Naangal
  , Nee, Neer, Neengal ]
  ++ map Third allThirdPersonSubjects

-- | Converts a subject to be more respectful, if possible
makeRespectful :: Subject -> Maybe Subject
makeRespectful = \case
  Naam -> Just Naangal
  Nee -> Just Neengal
  Neer -> Just Neengal
  Neengal -> Just Neengal
  Third Avan -> Just $ Third Avar
  Third Aval -> Just $ Third Avar
  Third Avar -> Just $ Third Avar
  Third Avargal -> Just $ Third Avargal
  _ -> Nothing

-- | Apply a suffix that doesn't have a special case for avai
commonSuffix :: Subject -> ChoiceString -> ChoiceString
commonSuffix subject root = usingAvaiSuffix subject \case
  False -> root
  True  -> root |+ "an"

-- | Calls the provided function with @False@ for non-avai suffixes and @True@ for avai suffixes and then joins them
usingAvaiSuffix :: Subject -> (Bool -> ChoiceString) -> ChoiceString
usingAvaiSuffix subject f =
  case subject of
    Naan    -> simple "En"
    Naam    -> simple "Om"
    Naangal -> simple "Om"
    Nee     -> simple "aay"
    Neer    -> simple "eer"
    Neengal -> simple "eergaL"
    -- These specal forms are occaisionally used in formal literature
    Third Avan ->
      (f False |+ "aan") <> demote (f True |+ "an")
    Third Aval ->
      (f False |+ "aaL") <> demote (f True |+ "aL")
    Third Avar ->
      (f False |+ "aar") <> demote (f True |+ "ar")
    Third Avargal ->
      (f False |+ "aargaL") <> demote (f True |+ "ar")
    -- Avai requires this form instead of -ana
    Third (Irrational Avai) ->
      f True |+ "a"
    -- Adhu's irregularities are handled separately
    Third (Irrational Adhu) ->
      simple "adhu"
  where
    simple = (f False |+)

-- | Gets the past tense stem of the verb
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

-- | Gets the special past tense forms (N and Y) for 'Class3' verbs
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
  in
    case getEnding $ verbRoot verb of
      LongVowel ->
        promote past <> demote withY
      Alveolar L ->
        promote past <> promote withY
      _ ->
        promote withY <> promote past

-- | Gets the adverb form of a verb
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

-- | Represents when to convert L's in the stem into their respective hard consonants
data StemKind
  = StemStrengthBased  -- ^ Convert when the verb is 'Strong'
  | StemPlain          -- ^ Never convert, even when 'Strong'

-- | Like @getStem@, but accepts a @StemKind@
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

-- | Gets the stem onto which the present and future suffixes are added
getStem :: Verb -> ChoiceString
getStem = getStemKind StemStrengthBased

-- | Adds either a single or double hard consonant to the stem depending on the strength of the verb
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

-- | Gets the present tense stem of the verb
getPresent :: Verb -> Bool -> ChoiceString
getPresent verb avai =
  if avai then
    oneOrTwoOnStem K verb |+ "indRan"
  else
    oneOrTwoOnStem K verb |+| ChoiceString ["iR"] ["indR"]

-- | Gets the future tense stem of the verb
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

-- | Gets the future tense of the verb for 'Adhu'
getFutureAdhu :: Verb -> ChoiceString
getFutureAdhu verb =
  case verbFutureAdhu verb of
    Just future -> future
    Nothing ->
      let stem = collapse $ getStem verb in
      case getStrength verb of
        Weak ->
          let
            basic = suffix stem "um"
            withGu = stem `append` "gum"
            withoutGu = stem `append` "m"
          in
            if endsInLongVowel stem then
                if isSingleLetter stem then
                  ChoiceString [withGu, withoutGu] []
                else
                  case verbClass verb of
                    Class3 ->
                      ChoiceString [withGu] [withoutGu]
                    _ ->
                      ChoiceString [withGu] [basic]
            else
              common basic
        Strong ->
          common $ stem `append`
            if endsInHardConsonant stem then
              "kum"
            else
              "kkum"

-- | Like @getInfinitiveRoot@, but accepts a @StemKind@
getInfinitiveRootKind :: StemKind -> Verb -> ChoiceString
getInfinitiveRootKind kind verb =
  case verbInfinitiveRoot verb of
    Just root -> root
    Nothing ->
      let stem = collapse $ getStemKind kind verb in
      case getStrength verb of
        Weak ->
          if endsInLongVowel stem then
            let basic = stem `append` "gu" in
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
                    "ku"
                  else
                    "kku"

-- | Like @getInfinitive@, but with U instead of the final A
getInfinitiveRoot :: Verb -> ChoiceString
getInfinitiveRoot verb =
  let plain = getInfinitiveRootKind StemPlain verb in
  case getStrength verb of
    Weak -> plain
    Strong ->
      getInfinitiveRootKind StemStrengthBased verb <> demote plain

-- | Gets the infinitive form of the verb
getInfinitive :: Verb -> ChoiceString
getInfinitive verb =
  getInfinitiveRootKind StemStrengthBased verb |+ "a"

-- | Gets the root from which the negative future for 'Adhu' will be formed
getNegativeFutureAdhuRoot :: Verb -> ChoiceString
getNegativeFutureAdhuRoot verb =
  getInfinitiveRoot verb <> demote (getRespectfulCommandRoot verb)

-- | Gets the negative future tense of the verb for an 'IrrationalSubject'
getNegativeFutureIrrational :: IrrationalSubject -> Verb -> ChoiceString
getNegativeFutureIrrational subject verb =
  let root = getNegativeFutureAdhuRoot verb in
  case subject of
    Adhu ->
      root |+ "aadhu"
    Avai ->
      root |+| ("aadhu" <> "aa")

-- | Gets the verbal noun formed by adding -adhu
getNounAdhu :: Verb -> ChoiceString
getNounAdhu = conjugateRelative Future $ Irrational Adhu

-- | Gets the verbal noun formed by adding -thal
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

-- | Gets the verbal noun formed by adding -kai
getNounKai :: Verb -> ChoiceString
getNounKai verb =
  oneOrTwoOnStem K verb |+ "ai"

-- | Gets the verbal noun formed by adding -al
getNounAl :: Verb -> ChoiceString
getNounAl verb =
  getInfinitive verb |+ "l"

-- | Gets the root from which the respectful command will be formed
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

-- | Represents a tense used for a 'PositiveConjugation'
data TenseConjugation
  = Past
  | Present
  | Future

instance Show TenseConjugation where
  show = \case
    Past    -> "past"
    Present -> "present"
    Future  -> "future"

-- | Conjugate a verb with a certain tense and subject
conjugateFinite :: TenseConjugation -> Subject -> Verb -> ChoiceString
conjugateFinite tense subject verb =
  case (tense, subject) of
    (Past, Third (Irrational Adhu)) | verbClass verb == Class3 ->
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
    (Past, _) ->
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
    (Present, _) ->
      usingAvaiSuffix subject $ getPresent verb
    (Future, Third (Irrational Adhu)) ->
      getFutureAdhu verb
    (Future, Third (Irrational Avai)) ->
      getFutureAdhu verb <> getFuture verb |+ "ana"
    (Future, _) ->
      commonSuffix subject $ getFuture verb

-- | Appends a suffix if past/present and returns a default value if future
getAdjective :: TenseConjugation -> Verb -> ChoiceString -> ChoiceString -> ChoiceString
getAdjective tense verb suffix future =
  case tense of
    Past
      | verbClass verb == Class3 ->
        getClass3PastNYForm verb |+| suffix
      | otherwise ->
        getPast verb |+| suffix
    Present ->
      getPresent verb False |+| suffix
    Future ->
      future

-- | Conjugate a verb as a relative clause
conjugateRelative :: TenseConjugation -> ThirdPersonSubject -> Verb -> ChoiceString
conjugateRelative tense subject verb =
  getAdjective tense verb (relativeSuffix subject) $
    promote $
      oneOrTwoOnStem P verb |+| pRelativeSuffix subject
      <> getFuture verb |+| demote (vRelativeSuffix subject)

-- | Represents whether a conjugation for a command should be respectful
type Respectful = Bool

-- | Represents a regular conjugation that is not negated
data PositiveConjugation
  = Finite TenseConjugation Subject               -- ^ (e.g. @irundhEn@, @irukkiREn@, @iruppEn@)
  | Adjective TenseConjugation                    -- ^ (e.g. @irundha@, @irukkiRa@, @irukkum@)
  | Relative TenseConjugation ThirdPersonSubject  -- ^ (e.g. @irundhavar@, @irukkiRavar@, @iruppavar@)
  | Noun                                          -- ^ (e.g. @iruppadhu@)
  | Adverb                                        -- ^ (e.g. @irundhu@)
  | Conditional                                   -- ^ (e.g. @irundhaal@)
  | Command Respectful                            -- ^ (e.g. @iru@, @irungaL@)

-- | Get a 'PositiveConjugation' of a 'Verb'
conjugatePositive :: PositiveConjugation -> Verb -> ChoiceString
conjugatePositive conjugation verb =
  case conjugation of
    Finite tense subject ->
      conjugateFinite tense subject verb
    Adjective tense ->
      getAdjective tense verb "a" $ getFutureAdhu verb
    Relative tense subject ->
      conjugateRelative tense subject verb
    Adverb ->
      getAdverb verb
    Noun ->
      getNounAdhu verb <> getNounThal verb <> getNounKai verb <> getNounAl verb
    Conditional ->
      getPast verb |+ "aal" <> demote (getInfinitiveRoot verb |+| ("il" <> "in"))
    Command False ->
      getRoot verb
    Command True ->
      let commandRoot = getRespectfulCommandRoot verb in
      commandRoot |+ "ngaL" <> demote (commandRoot |+ "m")

-- | Represents a negated conjugation
data NegativeConjugation
  = NegativePastPresent                 -- ^ (e.g. @seyyavillai@)
  | NegativeFuture Subject              -- ^ (e.g. @seyyamaaTTEn@)
  | NegativeClassical Subject           -- ^ (e.g. @seyyEn@)
  | NegativeAdjective                   -- ^ (e.g. @seyyaadha@)
  | NegativeRelative ThirdPersonSubject -- ^ (e.g. @seyyaadhavar@)
  | NegativeNoun                        -- ^ (e.g. @seyyaadhadhu@)
  | NegativeAdverb                      -- ^ (e.g. @seyyaamal@)
  | NegativeConditional                 -- ^ (e.g. @seyyaaviTTaal@)
  | NegativeCommand Respectful          -- ^ (e.g. @seyyaadhE@, @seyyaadheergaL@)

-- | A helper verb that is used for 'NegativeFuture' conjugations
maaTTu :: Verb
maaTTu = defaultVerb
  { verbClass = Class3
  , verbRoot = "maaTTu" }

-- | A helper verb that is used for 'NegativeConditional' conjugations
viDu :: Verb
viDu = defaultVerb
  { verbClass = Class1 Weak
  , verbRoot = "viDu" }

-- | Get a 'NegativeConjugation' of a 'Verb'
conjugateNegative :: NegativeConjugation -> Verb -> ChoiceString
conjugateNegative conjugation verb =
  case conjugation of
    NegativePastPresent ->
      getInfinitive verb |+ "illai"
    NegativeFuture (Third (Irrational irrational)) ->
      getNegativeFutureIrrational irrational verb
    NegativeFuture subject ->
      getInfinitive verb |+| conjugateNegative (NegativeClassical subject) maaTTu
    NegativeClassical (Third (Irrational irrational)) ->
      getNegativeFutureIrrational irrational verb
    NegativeClassical subject ->
      commonSuffix subject $ getRespectfulCommandRoot verb
    NegativeAdjective ->
      getNegativeFutureAdhuRoot verb |+| ChoiceString ["aadha"] ["aa"]
    NegativeRelative subject ->
      getNegativeFutureAdhuRoot verb |+ "aadhu" |+| relativeSuffix subject
    NegativeNoun ->
      getNegativeFutureAdhuRoot verb |+| ChoiceString ["aadhadhu"] ["aamai"]
    NegativeAdverb ->
      getNegativeFutureAdhuRoot verb |+| ChoiceString ["aamal"] ["aadhu", "aa"]
    NegativeConditional ->
      getNegativeFutureAdhuRoot verb |+| ChoiceString ["aa"] ["aamal"] |+| conjugatePositive Conditional viDu
    NegativeCommand False ->
      getNegativeFutureAdhuRoot verb |+ "aadhE"
    NegativeCommand True ->
      getNegativeFutureAdhuRoot verb |+ "aadheergaL"
        <> demote (getNegativeFutureAdhuRoot verb |+ "aadheer")

-- | Represents a way to conjugate a verb
data Conjugation
  = Positive PositiveConjugation
  | Negative NegativeConjugation
  | Infinitive  -- ^ (e.g. @irukka@)

-- | Get a 'Conjugation' of a 'Verb'
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

