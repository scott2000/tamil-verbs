module TamilString where

import GHC.Generics

import Control.Monad
import Control.Applicative

import Data.String
import Data.List
import Data.Maybe
import Data.Hashable

class TamilShow a where
  tamilShow :: a -> TamilString

data ChoiceString = ChoiceString
  { csCommon :: [TamilString]
  , csUncommon :: [TamilString] }

instance Semigroup ChoiceString where
  ChoiceString xc xu <> ChoiceString yc yu =
    ChoiceString (xc ++ yc) (xu ++ yu)

instance Monoid ChoiceString where
  mempty = ChoiceString [] []

instance Show ChoiceString where
  show = showUsing show

instance IsString ChoiceString where
  fromString = common . fromString

common :: TamilString -> ChoiceString
common s = ChoiceString [s] []

uncommon :: TamilString -> ChoiceString
uncommon s = ChoiceString [] [s]

hide :: ChoiceString -> ChoiceString
hide (ChoiceString [] u) = ChoiceString u []
hide (ChoiceString c  _) = ChoiceString c []

deduplicate :: ChoiceString -> ChoiceString
deduplicate (ChoiceString c u) =
  ChoiceString c' u'
    where
      c' = nub c
      u' = nub $ filter (`notElem` c') u

showUsing :: (TamilString -> String) -> ChoiceString -> String
showUsing _ (ChoiceString [] []) = "<none>"
showUsing showFunction str =
  intercalate "; " $ map (intercalate ", " . map showFunction) $ filter (not . null) [c, u]
  where
    ChoiceString c u = deduplicate str

(|+) :: ChoiceString -> TamilString -> ChoiceString
cs |+ s =
  forChoice cs \c -> suffix c s

(|+|) :: ChoiceString -> ChoiceString -> ChoiceString
ChoiceString xc xu |+| ChoiceString yc yu =
  ChoiceString cc (cu ++ uc ++ uu)
  where
    suffix' = flip $ liftA2 $ flip suffix
    cc = suffix' xc yc
    cu = suffix' xc yu
    uc = suffix' xu yc
    uu = suffix' xu yu

filterMapChoice :: ChoiceString -> (TamilString -> Maybe TamilString) -> ChoiceString
filterMapChoice (ChoiceString c u) f =
  ChoiceString (mapMaybe f c) (mapMaybe f u)

filterChoice :: ChoiceString -> (TamilString -> Bool) -> ChoiceString
filterChoice (ChoiceString c u) pred =
  ChoiceString (filter pred c) (filter pred u)

forChoice :: ChoiceString -> (TamilString -> TamilString) -> ChoiceString
forChoice (ChoiceString c u) f =
  ChoiceString (map f c) (map f u)

allChoices :: ChoiceString -> [TamilString]
allChoices str = c ++ u
  where
    ChoiceString c u = deduplicate str

collapse :: ChoiceString -> TamilString
collapse (ChoiceString (x:_) _) = x
collapse (ChoiceString _ (x:_)) = x
collapse (ChoiceString _ _) = error "collapse: no valid choices"

promote :: ChoiceString -> ChoiceString
promote (ChoiceString [] (c:u)) =
  ChoiceString [c] u
promote other = other

demote :: ChoiceString -> ChoiceString
demote (ChoiceString c u) = ChoiceString [] (c ++ u)

splitWith :: (Char -> Bool) -> String -> [String]
splitWith isBreak s =
  case dropWhile isBreak s of
    "" -> []
    s' ->
      let (w, s'') = break isBreak s' in
      w : splitWith isBreak s''

splitHyphen :: String -> [String]
splitHyphen = splitWith \case
  '-' -> True
  ',' -> True
  ' ' -> True
  _   -> False

split :: Char -> String -> [String]
split = splitWith . (==)

data VowelLength
  = Short
  | Long
  deriving (Ord, Eq, Generic)

instance Hashable VowelLength

data Vowel
  = A !VowelLength
  | I !VowelLength
  | U !VowelLength
  | E !VowelLength
  | Ai
  | O !VowelLength
  | Au
  deriving (Ord, Eq, Generic)

instance Hashable Vowel

instance Show Vowel where
  show = \case
    A Short -> "அ (a)"
    A Long  -> "ஆ (aa)"
    I Short -> "இ (i)"
    I Long  -> "ஈ (ee)"
    U Short -> "உ (u)"
    U Long  -> "ஊ (oo)"
    E Short -> "எ (e)"
    E Long  -> "ஏ (E)"
    Ai      -> "ஐ (ai)"
    O Short -> "ஒ (o)"
    O Long  -> "ஓ (O)"
    Au      -> "ஔ (au)"

vowelInitial :: Vowel -> Char
vowelInitial = \case
  A Short -> 'அ'
  A Long  -> 'ஆ'
  I Short -> 'இ'
  I Long  -> 'ஈ'
  U Short -> 'உ'
  U Long  -> 'ஊ'
  E Short -> 'எ'
  E Long  -> 'ஏ'
  Ai      -> 'ஐ'
  O Short -> 'ஒ'
  O Long  -> 'ஓ'
  Au      -> 'ஔ'

vowelCombining :: Vowel -> Maybe Char
vowelCombining = \case
  A Short -> Nothing
  A Long  -> Just '\x0bbe'
  I Short -> Just '\x0bbf'
  I Long  -> Just '\x0bc0'
  U Short -> Just '\x0bc1'
  U Long  -> Just '\x0bc2'
  E Short -> Just '\x0bc6'
  E Long  -> Just '\x0bc7'
  Ai      -> Just '\x0bc8'
  O Short -> Just '\x0bca'
  O Long  -> Just '\x0bcb'
  Au      -> Just '\x0bcc'

pattern Dot :: Char
pattern Dot = '\x0bcd'

isShortVowel :: Vowel -> Bool
isShortVowel = \case
  A Short -> True
  I Short -> True
  U Short -> True
  E Short -> True
  O Short -> True
  _       -> False

isShortishVowel :: Vowel -> Bool
isShortishVowel = \case
  Ai -> True
  Au -> True
  v  -> isShortVowel v

isShortish :: TamilString -> Bool
isShortish (TamilString str) =
  case str of
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

data Vallinam
  = K
  | Ch
  | TRetroflex
  | TDental
  | P
  | RAlveolar
  deriving (Ord, Eq, Generic)

instance Hashable Vallinam

instance Show Vallinam where
  show = \case
    K          -> "க் (k/g)"
    Ch         -> "ச் (ch/s)"
    TRetroflex -> "ட் (T/D)"
    TDental    -> "த் (t/d)"
    P          -> "ப் (p/b)"
    RAlveolar  -> "ற் (R)"

getPaired :: Vallinam -> Mellinam
getPaired = \case
  K          -> Ng
  Ch         -> Ny
  TRetroflex -> NRetroflex
  TDental    -> NDental
  P          -> M
  RAlveolar  -> NAlveolar

data Mellinam
  = Ng
  | Ny
  | NRetroflex
  | NDental
  | M
  | NAlveolar
  deriving (Ord, Eq, Generic)

instance Hashable Mellinam

instance Show Mellinam where
  show = \case
    Ng         -> "ங் (ng~)"
    Ny         -> "ஞ் (ny)"
    NRetroflex -> "ண் (N)"
    NDental    -> "ந் (nh)"
    M          -> "ம் (m)"
    NAlveolar  -> "ன் (n)"

data Idaiyinam
  = Y
  | R
  | LAlveolar
  | V
  | Zh
  | LRetroflex
  deriving (Ord, Eq, Generic)

instance Hashable Idaiyinam

instance Show Idaiyinam where
  show = \case
    Y          -> "ய் (y)"
    R          -> "ர் (r)"
    LAlveolar  -> "ல் (l)"
    V          -> "வ் (v)"
    Zh         -> "ழ் (zh)"
    LRetroflex -> "ள் (L)"

data Grantha
  = J
  | Sh
  | S
  | H
  | SSh
  deriving (Ord, Eq, Generic)

instance Hashable Grantha

instance Show Grantha where
  show = \case
    J   -> "ஜ் (J)"
    Sh  -> "ஷ் (Sh)"
    S   -> "ஸ் (S)"
    H   -> "ஹ் (H)"
    SSh -> "ஶ் (SSh)"

data Consonant
  = Hard !Vallinam
  | Soft !Mellinam
  | Medium !Idaiyinam
  | Grantha !Grantha
  deriving (Ord, Eq, Generic)

instance Hashable Consonant

instance Show Consonant where
  show = \case
    Hard h    -> show h
    Soft s    -> show s
    Medium m  -> show m
    Grantha g -> show g

consonantWithA :: Consonant -> Char
consonantWithA = \case
  Hard K            -> 'க'
  Hard Ch           -> 'ச'
  Hard TRetroflex   -> 'ட'
  Hard TDental      -> 'த'
  Hard P            -> 'ப'
  Hard RAlveolar    -> 'ற'
  Soft Ng           -> 'ங'
  Soft Ny           -> 'ஞ'
  Soft NRetroflex   -> 'ண'
  Soft NDental      -> 'ந'
  Soft M            -> 'ம'
  Soft NAlveolar    -> 'ன'
  Medium Y          -> 'ய'
  Medium R          -> 'ர'
  Medium LAlveolar  -> 'ல'
  Medium V          -> 'வ'
  Medium Zh         -> 'ழ'
  Medium LRetroflex -> 'ள'
  Grantha J         -> 'ஜ'
  Grantha Sh        -> 'ஷ'
  Grantha S         -> 'ஸ'
  Grantha H         -> 'ஹ'
  Grantha SSh       -> 'ஶ'

mayDouble :: Consonant -> Bool
mayDouble = \case
  Soft _            -> True
  Medium LRetroflex -> True
  Medium LAlveolar  -> True
  _                 -> False

data TamilLetter
  = Vowel !Vowel
  | Consonant !Consonant
  | Aaydham
  deriving (Ord, Eq, Generic)

instance Hashable TamilLetter

instance Show TamilLetter where
  show = \case
    Vowel v     -> show v
    Consonant c -> show c
    Aaydham     -> "ஃ (K)"

newtype TamilString = TamilString
  { -- | A reversed list of letters
    untamil :: [TamilLetter] }
  deriving (Ord, Eq, Generic)

instance Hashable TamilString

instance Show TamilString where
  show str = toTamil str ++ " (" ++ toLatin str ++ ")"

instance IsString TamilString where
  fromString str =
    case parseTamilSuffix str of
      Left err ->
        error err
      Right res ->
        res

parseTamilSuffix :: String -> Either String TamilString
parseTamilSuffix str = TamilString <$> foldM convert [] str
  where
    convert str = \case
      'a' -> vowelCombinations [(A Short, A Long)] $ A Short
      'A' -> vowel $ A Long
      'i' -> vowelCombinations [(A Short, Ai)] $ I Short
      'I' -> vowel $ I Long
      'u' -> vowelCombinations [(A Short, Au)] $ U Short
      'U' -> vowel $ U Long
      'e' -> vowelCombinations [(E Short, I Long), (A Short, E Long)] $ E Short
      'E' -> vowel $ E Long
      'o' -> vowelCombinations [(O Short, U Long)] $ O Short
      'O' -> vowel $ O Long

      'k' -> consonant $ Hard K
      'g' -> voiced K

      'c' -> consonant $ Hard Ch
      's' -> consonant $ Hard Ch
      'j' -> voiced Ch

      'T' -> consonant $ Hard TRetroflex
      'D' -> consonant $ Hard TRetroflex
      'N' -> consonant $ Soft NRetroflex

      't' -> consonant $ Hard TDental
      'd' -> voiced TDental

      'p' -> consonant $ Hard P
      'b' -> consonant $ Hard P
      'm' -> consonant $ Soft M

      'y' ->
        case str of
          Consonant (Soft NAlveolar) : rest ->
            -- ny
            Right $ Consonant (Soft Ny) : rest
          _ ->
            consonant $ Medium Y

      'Y' -> consonant $ Medium Y
      'r' -> r $ Medium R
      'l' -> consonant $ Medium LAlveolar
      'v' -> consonant $ Medium V
      'w' -> consonant $ Medium V
      'z' -> consonant $ Medium Zh
      'L' -> consonant $ Medium LRetroflex

      'R' -> r $ Hard RAlveolar
      'n' -> consonant $ Soft NAlveolar

      'J' -> consonant $ Grantha J
      'S' -> consonant $ Grantha S
      'H' -> consonant $ Grantha H
      'K' -> aaydham

      'F' -> aaydhamCombine $ Hard P
      'Z' -> aaydhamCombine $ Grantha J
      'X' -> aaydhamCombine $ Grantha S

      'h' ->
        case str of
          Vowel (O Short) : rest ->
            -- oh = O
            Right $ Vowel (O Long) : rest
          Consonant (Hard _) : _ ->
            -- kh, ch, th, dh, bh, etc.
            Right str
          Consonant (Soft NAlveolar) : rest ->
            Right case rest of
              [] -> str
              _ ->
                -- nh, only when it's not the first letter
                Consonant (Soft NDental) : rest
          Consonant (Medium Zh) : _ ->
            -- zh
            Right str
          Consonant (Grantha S) : Consonant (Grantha S) : rest ->
            -- SSh
            Right $ Consonant (Grantha SSh) : rest
          Consonant (Grantha S) : rest ->
            -- Sh
            Right $ Consonant (Grantha Sh) : rest
          _ ->
            consonant $ Hard K

      '~' ->
        case str of
          Consonant (Hard K) : rest@(Consonant (Soft Ng) : _) ->
            Right rest
          _ ->
            Left "'~' can only occur after 'ng'"

      'அ' -> vowel $ A Short
      'ஆ' -> vowel $ A Long
      'இ' -> vowel $ I Short
      'ஈ' -> vowel $ I Long
      'உ' -> vowel $ U Short
      'ஊ' -> vowel $ U Long
      'எ' -> vowel $ E Short
      'ஏ' -> vowel $ E Long
      'ஐ' -> vowel Ai
      'ஒ' -> vowel $ O Short
      'ஓ' -> vowel $ O Long
      'ஔ' -> vowel Au

      '\x0bbe' ->
        case str of
          Vowel v : rest ->
            case v of
              A Short ->
                -- Manually handle the case that is normally handled by vowelA
                Right $ Vowel (A Long) : rest
              U Short ->
                -- A long U can sometimes look like a short U followed by A
                Right $ Vowel (U Long) : rest
              E len ->
                -- Split marking option in Unicode for O
                Right $ Vowel (O len) : rest
              _ ->
                -- Otherwise, the marking for A can look like an R sometimes
                consonantA $ Medium R
          _ ->
            vowel $ A Long

      '\x0bbf' -> vowelA $ I Short
      '\x0bc0' -> vowelA $ I Long
      '\x0bc1' -> vowelA $ U Short
      '\x0bc2' -> vowelA $ U Long
      '\x0bc6' -> vowelA $ E Short
      '\x0bc7' -> vowelA $ E Long
      '\x0bc8' -> vowelA Ai
      '\x0bca' -> vowelA $ O Short
      '\x0bcb' -> vowelA $ O Long
      '\x0bcc' -> vowelA Au

      '\x0bd7' ->
        -- Split marking option in Unicode for Au
        case str of
          Vowel (E Short) : rest ->
            Right $ Vowel Au : rest
          Vowel (O Short) : rest ->
            Right $ Vowel Au : rest
          _ ->
            -- Otherwise, the marking looks like an L
            consonantA $ Medium LRetroflex

      Dot ->
        case str of
          Vowel (A Short) : rest ->
            Right rest
          _ ->
            Left "pulli can only occur after 'a'"

      'க' -> consonantA $ Hard K
      'ச' -> consonantA $ Hard Ch
      'ட' -> consonantA $ Hard TRetroflex
      'த' -> consonantA $ Hard TDental
      'ப' -> consonantA $ Hard P
      'ற' -> consonantA $ Hard RAlveolar
      'ங' -> consonantA $ Soft Ng
      'ஞ' -> consonantA $ Soft Ny
      'ண' -> consonantA $ Soft NRetroflex
      'ந'
        | null str ->
          consonantA $ Soft NAlveolar
        | otherwise ->
          consonantA $ Soft NDental
      'ம' -> consonantA $ Soft M
      'ன' -> consonantA $ Soft NAlveolar
      'ய' -> consonantA $ Medium Y
      'ர' -> consonantA $ Medium R
      'ல' -> consonantA $ Medium LAlveolar
      'வ' -> consonantA $ Medium V
      'ழ' -> consonantA $ Medium Zh
      'ள' -> consonantA $ Medium LRetroflex
      'ஜ' -> consonantA $ Grantha J
      'ஷ' -> consonantA $ Grantha Sh
      'ஸ' -> consonantA $ Grantha S
      'ஹ' -> consonantA $ Grantha H
      'ஶ' -> consonantA $ Grantha SSh
      'ஃ' -> aaydham

      ' ' ->
        Left "word cannot contain spaces"
      '\n' ->
        Left "word cannot contain newlines"
      ch ->
        Left $ "not valid Tamil letter: " ++ [ch]
      where
        vowelCombinations combinations current =
          case str of
            Vowel ch : rest ->
              let
                go [] = vowel current
                go ((previous, combination) : combinations)
                  | previous == ch =
                    Right $ Vowel combination : rest
                  | otherwise =
                    go combinations
              in
                go combinations
            _ ->
              vowel current
        vowel v =
          case str of
            Vowel (O Long) : rest ->
              -- oha -> Oa -> oga
              Right $ Vowel v : Consonant (Hard K) : Vowel (O Short) : rest
            Vowel _ : _ ->
              Left "vowel cannot immediately follow vowel"
            _ ->
              Right $ Vowel v : str
        consonant c =
          Right $ Consonant c : str
        aaydham =
          Right $ Aaydham : str
        aaydhamCombine c =
          Right $ Consonant c : Aaydham : str
        voiced c =
          case str of
            Consonant (Soft NAlveolar) : rest ->
              Right $ Consonant (Hard c) : Consonant (Soft $ getPaired c) : rest
            _ ->
              consonant $ Hard c
        vowelA v =
          case str of
            Vowel (A Short) : rest ->
              Right $ Vowel v : rest
            _ ->
              vowel v
        consonantA c =
          Right $ Vowel (A Short) : Consonant c : str
        r c =
          case str of
            Consonant (Hard TDental) : Consonant (Soft NDental) : rest ->
              -- ndr
              Right $ Consonant (Hard RAlveolar) : Consonant (Soft NAlveolar) : rest
            Consonant (Hard TDental) : rest ->
              -- tr
              Right $ Consonant (Hard RAlveolar) : Consonant (Hard RAlveolar) : rest
            _ ->
              consonant c

parseTamil :: String -> Either String TamilString
parseTamil str =
  convertInitialN <$> parseTamilSuffix str

convertInitialN :: TamilString -> TamilString
convertInitialN =
  TamilString . go . untamil
  where
    go [] = []
    go [Consonant (Soft NAlveolar)] = [Consonant $ Soft NDental]
    go (ch : rest) = ch : go rest

toLatin :: TamilString -> String
toLatin (TamilString str) =
  case str of
    Consonant (Hard h) : rest@(Vowel _ : _) ->
      go [unvoiced h] rest
    Consonant (Hard h) : rest@(Consonant (Medium _) : _) ->
      go [unvoiced h] rest
    _ ->
      go "" str
  where
    vowel = \case
      A Short -> "a"
      A Long  -> "aa"
      I Short -> "i"
      I Long  -> "ee"
      U Short -> "u"
      U Long  -> "oo"
      E Short -> "e"
      E Long  -> "E"
      Ai      -> "ai"
      O Short -> "o"
      O Long  -> "O"
      Au      -> "au"
    unvoiced = \case
      K          -> 'k'
      Ch         -> 'c'
      TRetroflex -> 'T'
      TDental    -> 't'
      P          -> 'p'
      RAlveolar  -> 'R'
    voiced = \case
      K          -> "g"
      Ch         -> "s"
      TRetroflex -> "D"
      TDental    -> "dh"
      P          -> "b"
      RAlveolar  -> "R"
    willConvertNasal = \case
      K       -> True
      TDental -> True
      _       -> False
    soft = \case
      Ng         -> "ng~"
      Ny         -> "ny"
      NRetroflex -> "N"
      NDental    -> "nh"
      M          -> "m"
      NAlveolar  -> "n"
    grantha = \case
      J   -> "J"
      Sh  -> "Sh"
      S   -> "S"
      H   -> "H"
      SSh -> "SSh"
    go acc str =
      case str of
        [] -> acc
        [Vowel v, Consonant (Hard TDental)] | v /= I Short, v /= I Long, v /= E Short, v /= E Long ->
          "th" ++ vowel v ++ acc
        Vowel v : rest ->
          go' rest $ vowel v
        Consonant (Hard P) : Aaydham : rest ->
          go' rest "F"
        Consonant (Grantha J) : Aaydham : rest ->
          go' rest "Z"
        Consonant (Grantha S) : Aaydham : rest ->
          go' rest "X"
        Consonant (Hard h) : Consonant (Soft s) : rest
          | s == getPaired h ->
            go' rest case h of
              K          -> "ng"
              Ch         -> "nj"
              TRetroflex -> "ND"
              TDental    -> "ndh"
              P          -> "mb"
              RAlveolar  -> "ndR"
          | NAlveolar <- s, willConvertNasal h ->
            go' rest $ soft s ++ [unvoiced h]
          | otherwise ->
            go' rest $ soft s ++ voiced h
        Consonant (Hard RAlveolar) : Consonant (Hard RAlveolar) : rest ->
          go' rest "tR"
        Consonant (Hard Ch) : Consonant (Hard o) : rest ->
          go' rest (unvoiced o : "ch")
        Consonant (Hard h) : Consonant (Hard o) : rest ->
          go' rest [unvoiced o, unvoiced h]
        [Consonant (Hard Ch)] ->
          's' : acc
        [Consonant (Hard h)] ->
          unvoiced h : acc
        Consonant (Hard h) : rest ->
          go' rest $ voiced h
        [Consonant (Soft NDental)] ->
          'n' : acc
        Consonant (Soft s) : rest ->
          go' rest $ soft s
        Consonant (Medium Y) : Consonant (Soft NAlveolar) : rest ->
          go' rest "nY"
        Consonant (Medium m) : rest ->
          go' rest case m of
            Y          -> "y"
            R          -> "r"
            LAlveolar  -> "l"
            V          -> "v"
            Zh         -> "zh"
            LRetroflex -> "L"
        Consonant (Grantha g) : Consonant (Hard h) : rest ->
          go' rest $ unvoiced h : grantha g
        Consonant (Grantha g) : rest ->
          go' rest $ grantha g
        Aaydham : rest ->
          go' rest "K"
      where
        go' rest str = go (str ++ acc) rest

toTamil :: TamilString -> String
toTamil (TamilString str) =
  go "" str
  where
    go acc str =
      case str of
        [] -> acc
        Consonant c : rest ->
          go (consonantWithA c : Dot : acc) rest
        Vowel v : Consonant c : rest ->
          case vowelCombining v of
            Nothing ->
              go (consonantWithA c : acc) rest
            Just vc ->
              go (consonantWithA c : vc : acc) rest
        Vowel v : rest ->
          go (vowelInitial v : acc) rest
        Aaydham : rest ->
          go ('ஃ' : acc) rest

normalize :: TamilString -> TamilString
normalize =
  TamilString . map go . untamil
  where
    go = \case
      Vowel (A _) -> Vowel $ A Short
      Vowel (U _) -> Vowel $ U Short
      Vowel (O _) -> Vowel $ U Short
      Vowel _     -> Vowel $ I Short
      Consonant (Hard TRetroflex)   -> Consonant $ Hard TDental
      Consonant (Hard RAlveolar)    -> Consonant $ Medium R
      Consonant (Soft Ng)           -> Consonant $ Hard K
      Consonant (Soft Ny)           -> Consonant $ Hard Ch
      Consonant (Soft M)            -> Consonant $ Hard P
      Consonant (Soft _)            -> Consonant $ Hard TDental
      Consonant (Medium Zh)         -> Consonant $ Medium R
      Consonant (Medium LRetroflex) -> Consonant $ Medium LAlveolar
      Consonant (Grantha H)         -> Consonant $ Hard K
      Consonant (Grantha _)         -> Consonant $ Hard Ch
      Aaydham                       -> Consonant $ Hard K
      other -> other

data FollowClass
  = FollowClassA
  | FollowClassB
  | FollowClassC
  | FollowClassD
  | FollowClassE
  | FollowClassAlways
  deriving Eq

getFollowClass :: Consonant -> FollowClass
getFollowClass = \case
  Hard K            -> FollowClassA
  Hard Ch           -> FollowClassA
  Hard TRetroflex   -> FollowClassC
  Hard TDental      -> FollowClassA
  Hard P            -> FollowClassA
  Hard RAlveolar    -> FollowClassC
  Soft Ng           -> FollowClassA
  Soft Ny           -> FollowClassB
  Soft NRetroflex   -> FollowClassD
  Soft NDental      -> FollowClassB
  Soft M            -> FollowClassB
  Soft NAlveolar    -> FollowClassD
  Medium Y          -> FollowClassE
  Medium R          -> FollowClassE
  Medium LAlveolar  -> FollowClassD
  Medium V          -> FollowClassB
  Medium Zh         -> FollowClassE
  Medium LRetroflex -> FollowClassD
  Grantha _         -> FollowClassAlways

getPreceding :: Consonant -> [FollowClass]
getPreceding = \case
  Hard K            -> allowCDE
  Hard Ch           -> allowCDE
  Hard TRetroflex   -> allowNone
  Hard TDental      -> allowE
  Hard P            -> allowCDE
  Hard RAlveolar    -> allowNone
  Soft Ng           -> allowE
  Soft Ny           -> allowDE
  Soft NRetroflex   -> allowNone
  Soft NDental      -> allowE
  Soft M            -> allowDE
  Soft NAlveolar    -> allowNone
  Medium Y          -> allowBDE
  Medium R          -> allowNone
  Medium LAlveolar  -> allowNone
  Medium V          -> allowBDE
  Medium Zh         -> allowNone
  Medium LRetroflex -> allowNone
  Grantha _         -> allowAll
  where
    allowNone = [FollowClassAlways]
    allowE    = FollowClassE : allowNone
    allowDE   = FollowClassD : allowE
    allowCDE  = FollowClassC : allowDE
    allowBDE  = FollowClassB : allowDE
    allowAll  = FollowClassA : FollowClassB : allowCDE

allowJunction :: Consonant -> Consonant -> Bool
allowJunction (Medium Zh) (Medium Zh) = False
allowJunction (Soft s) (Hard h)
  | s == getPaired h = True
allowJunction first next
  | first == next = True
  | otherwise =
    getFollowClass first `elem` getPreceding next

getAlternativeJunction :: Consonant -> Consonant -> Maybe (Bool, Consonant, Consonant)
getAlternativeJunction (Soft M) (Hard h) =
  Just (True, Soft $ getPaired h, Hard h)
getAlternativeJunction end (Hard TDental) =
  case end of
    Hard TRetroflex   -> Just (True,  Hard TRetroflex, Hard TRetroflex)
    Hard RAlveolar    -> Just (True,  Hard RAlveolar,  Hard RAlveolar)
    Soft NRetroflex   -> Just (False, Soft NRetroflex, Hard TRetroflex)
    Soft NAlveolar    -> Just (False, Soft NAlveolar,  Hard RAlveolar)
    Medium LAlveolar  -> Just (False, Hard RAlveolar,  Hard RAlveolar)
    Medium LRetroflex -> Just (False, Hard TRetroflex, Hard TRetroflex)
    _                 -> Nothing
getAlternativeJunction end (Soft NDental) =
  case end of
    Soft NRetroflex   -> Just (True,  Soft NRetroflex, Soft NRetroflex)
    Soft M            -> Just (True,  Soft NDental,    Soft NDental)
    Soft NAlveolar    -> Just (True,  Soft NAlveolar,  Soft NAlveolar)
    Medium LAlveolar  -> Just (False, Soft NAlveolar,  Soft NAlveolar)
    Medium LRetroflex -> Just (False, Soft NRetroflex, Soft NRetroflex)
    _                 -> Nothing
getAlternativeJunction _ _ = Nothing

validateConsonantCluster :: [Consonant] -> Bool
validateConsonantCluster [] = True
validateConsonantCluster [_] = True
validateConsonantCluster [_, _] = True
validateConsonantCluster [Medium _, Soft _, Hard _] = True
validateConsonantCluster [Medium _, Hard _, Hard _] = True
validateConsonantCluster [Grantha _, _, _] = True
validateConsonantCluster [_, Grantha _, _] = True
validateConsonantCluster [_, _, Grantha _] = True
validateConsonantCluster _ = False

validateTamil :: TamilString -> Either String ()
validateTamil str = do
  go $ reverse $ untamil str
  forM_ (getConsonantClusters str) \cluster ->
    if validateConsonantCluster cluster then
      Right ()
    else
      Left $ "consonant cluster " ++ showCluster cluster ++ " is not allowed in Tamil"
  where
    showCluster cluster =
      let str = TamilString $ map Consonant $ reverse cluster in
      toTamil str ++ " (" ++ toLatin str ++ ")"
    go (current : rest@(next : _)) =
      case (current, next) of
        (Vowel _, Vowel _) ->
          Left "vowel cannot immediately follow vowel"
        (Consonant a, Consonant b) | not $ allowJunction a b ->
          if a == b then
            Left $ "consonant " ++ show a ++ " cannot be doubled"
          else
            case getAlternativeJunction a b of
              Nothing ->
                Left $ "consonant " ++ show a ++ " cannot be followed by " ++ show b
              Just (_, x, y)
                | y == b ->
                  Left $ "consonant " ++ show a ++ " should become " ++ show x ++ " when followed by " ++ show b
                | x == a ->
                  Left $ "consonant " ++ show b ++ " should become " ++ show y ++ " when preceded by " ++ show a
                | otherwise ->
                  Left $ "consonant cluster " ++ showCluster [a, b] ++ " should become " ++ showCluster [x, y]
        _ -> go rest
    go _ = Right ()

parseAndValidateTamil :: String -> Either String TamilString
parseAndValidateTamil str = do
  word <- parseTamil str
  validateTamil word
  return word

getConsonantClusters :: TamilString -> [[Consonant]]
getConsonantClusters = go [] [] . untamil
  where
    go cluster clusters = \case
      [] -> cluster : clusters
      (Consonant c : rest) ->
        go (c : cluster) clusters rest
      (_ : rest) ->
        go [] (cluster : clusters) rest

fromLetter :: TamilLetter -> TamilString
fromLetter = TamilString . (: [])

append :: TamilString -> TamilString -> TamilString
append (TamilString root) (TamilString suffix) =
  TamilString $ suffix ++ root

appendLetter :: TamilString -> TamilLetter -> TamilString
appendLetter (TamilString root) suffix =
  TamilString $ suffix : root

isSingleLetter :: TamilString -> Bool
isSingleLetter = \case
  TamilString [_] -> True
  _ -> False

endsInHardConsonant :: TamilString -> Bool
endsInHardConsonant = \case
  TamilString (Consonant (Hard _) : _) -> True
  _ -> False

endsInSoftConsonant :: TamilString -> Bool
endsInSoftConsonant = \case
  TamilString (Consonant (Soft _) : _) -> True
  _ -> False

endsInDoublingConsonant :: TamilString -> Bool
endsInDoublingConsonant = \case
  TamilString (Consonant c : _) -> mayDouble c
  _ -> False

endsInLongVowel :: TamilString -> Bool
endsInLongVowel = \case
  TamilString (Vowel v : _) -> not $ isShortishVowel v
  _ -> False

stripSuffix :: TamilString -> TamilString -> Maybe TamilString
stripSuffix (TamilString suffix) (TamilString str) =
  TamilString <$> stripPrefix suffix str

replaceLastLetter :: TamilString -> TamilString -> TamilString
replaceLastLetter (TamilString root) replacement =
  case root of
    (Vowel _ : Consonant _ : rest) ->
      addRest rest
    (Consonant _ : rest) ->
      addRest rest
    _ ->
      error "replaceLastLetter: expected a consonant or consonant-vowel letter at end of word"
  where
    addRest rest =
      case rest of
        Consonant _ : _ ->
          error "replaceLastLetter: cannot have doubled consonant"
        _ ->
          suffix (TamilString rest) replacement

suffix :: TamilString -> TamilString -> TamilString
suffix (TamilString root) (TamilString suffix) =
  TamilString $ go root suffix
  where
    go []              suffix = suffix
    go root            []     = root
    go root@(end:rest) suffix =
      case (last suffix, end) of
        (_, Aaydham) ->
          suffix ++ root
        (Consonant b, Consonant a)
          | Just (True, x, y) <- getAlternativeJunction a b ->
            init suffix ++ [Consonant y, Consonant x] ++ rest
        (Consonant _, _) ->
          suffix ++ root
        (_, Consonant c)
          | restIsShort, c /= Medium R, c /= Medium Zh ->
            middle c
          | otherwise ->
            suffix ++ root
        (Vowel (I Short), Vowel v) | not $ isShortishVowel v ->
          middle $ Medium Y
        (_, Vowel v) ->
          case v of
            U Short ->
              suffix ++ rest
            I _ ->
              middle $ Medium Y
            E _ ->
              middle $ Medium Y
            Ai ->
              middle $ Medium Y
            _ ->
              middle $ Medium V
      where
        middle c = suffix ++ Consonant c : root
        restIsShort =
          case rest of
            [Vowel v] ->
              isShortVowel v
            [Vowel v, Consonant _] ->
              isShortVowel v
            _ ->
              False

