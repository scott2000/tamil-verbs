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
  { csBest :: [TamilString]
  , csCommon :: [TamilString]
  , csOther :: [TamilString] }

instance Semigroup ChoiceString where
  ChoiceString xb xc xo <> ChoiceString yb yc yo =
    ChoiceString (xb ++ yb) (xc ++ yc) (xo ++ yo)

instance Monoid ChoiceString where
  mempty = ChoiceString [] [] []

instance Show ChoiceString where
  show (ChoiceString [] [] []) = "<none>"
  show (ChoiceString [] [] o) =
    intercalate ", " $ map show o
  show (ChoiceString b c _) =
    intercalate "; " $ map (intercalate ", " . map show) $ filter (not . null) [b, c]

instance IsString ChoiceString where
  fromString = best . fromString

best :: TamilString -> ChoiceString
best s = ChoiceString [s] [] []

common :: TamilString -> ChoiceString
common s = ChoiceString [] [s] []

other :: TamilString -> ChoiceString
other s = ChoiceString [] [] [s]

(|+) :: ChoiceString -> TamilString -> ChoiceString
cs |+ s =
  forChoice cs \c -> suffix c s

(|+|) :: ChoiceString -> ChoiceString -> ChoiceString
ChoiceString xb xc xo |+| ChoiceString yb yc yo =
  ChoiceString bb (bc ++ cb ++ cc) (bo ++ co ++ ob ++ oc ++ oo)
  where
    bb = liftA2 suffix xb yb
    bc = liftA2 suffix xb yc
    bo = liftA2 suffix xb yo
    cb = liftA2 suffix xc yb
    cc = liftA2 suffix xc yc
    co = liftA2 suffix xc yo
    ob = liftA2 suffix xo yb
    oc = liftA2 suffix xo yc
    oo = liftA2 suffix xo yo

filterMapChoice :: ChoiceString -> (TamilString -> Maybe TamilString) -> ChoiceString
filterMapChoice (ChoiceString b c o) f =
  ChoiceString (mapMaybe f b) (mapMaybe f c) (mapMaybe f o)

filterChoice :: ChoiceString -> (TamilString -> Bool) -> ChoiceString
filterChoice (ChoiceString b c o) pred =
  ChoiceString (filter pred b) (filter pred c) (filter pred o)

forChoice :: ChoiceString -> (TamilString -> TamilString) -> ChoiceString
forChoice (ChoiceString b c o) f =
  ChoiceString (map f b) (map f c) (map f o)

collapse :: ChoiceString -> TamilString
collapse (ChoiceString (x:_) _ _) = x
collapse (ChoiceString _ (x:_) _) = x
collapse (ChoiceString _ _ (x:_)) = x
collapse (ChoiceString _ _ _) = error "collapse: no valid choices"

promote :: ChoiceString -> ChoiceString
promote (ChoiceString [] (b:c) o) =
  ChoiceString [b] c o
promote (ChoiceString [] [] (b:o)) =
  ChoiceString [b] [] o
promote other = other

halfPromote :: ChoiceString -> ChoiceString
halfPromote (ChoiceString b@(_:_) c o) =
  ChoiceString [] (b ++ c) o
halfPromote (ChoiceString [] [] (c:o)) =
  ChoiceString [] [c] o
halfPromote other = other

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

data Vallinam
  = K
  | Ch
  | TRetroflex
  | TDental
  | P
  | RAlveolar
  deriving (Ord, Eq, Generic)

instance Hashable Vallinam

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

data Idaiyinam
  = Y
  | R
  | LAlveolar
  | V
  | Zh
  | LRetroflex
  deriving (Ord, Eq, Generic)

instance Hashable Idaiyinam

data Consonant
  = Hard !Vallinam
  | Soft !Mellinam
  | Medium !Idaiyinam
  deriving (Ord, Eq, Generic)

instance Hashable Consonant

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

data TamilLetter
  = Vowel !Vowel
  | Consonant !Consonant
  deriving (Ord, Eq, Generic)

instance Hashable TamilLetter

newtype TamilString = TamilString
  { -- | A reversed list of letters
    untamil :: [TamilLetter] }
  deriving (Ord, Eq, Generic)

instance Hashable TamilString

instance Show TamilString where
  show str = toTamil str ++ " (" ++ toLatin str ++ ")"

instance IsString TamilString where
  fromString str =
    case parseTamil str of
      Left err ->
        error $ show err
      Right res ->
        res

data ParseError
  = VowelHiatus
  | UnexpectedPulli
  | UnexpectedTilde
  | UnknownChar Char

instance Show ParseError where
  show = \case
    VowelHiatus ->
      "vowel cannot immediately follow vowel"
    UnexpectedPulli ->
      "pulli can only occur after 'a'"
    UnexpectedTilde ->
      "'~' can only occur after 'ng'"
    UnknownChar ' ' ->
      "word cannot contain spaces"
    UnknownChar '\n' ->
      "word cannot contain newlines"
    UnknownChar ch ->
      "not valid Tamil letter: " ++ [ch]

parseTamil :: String -> Either ParseError TamilString
parseTamil str = TamilString <$> foldM convert [] str
  where
    convert str = \case
      'a' -> vowelCombinations [(A Short, A Long)] $ A Short
      'A' -> vowel $ A Long
      'i' -> vowelCombinations [(A Short, Ai), (E Short, Ai)] $ I Short
      'I' -> vowel $ I Long
      'u' -> vowelCombinations [(A Short, Au), (O Short, Au)] $ U Short
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

      'r' -> r $ Medium R
      'l' -> consonant $ Medium LAlveolar
      'v' -> consonant $ Medium V
      'w' -> consonant $ Medium V
      'z' -> consonant $ Medium Zh
      'L' -> consonant $ Medium LRetroflex

      'R' -> r $ Hard RAlveolar
      'n' -> consonant $ Soft NAlveolar

      'h' ->
        case str of
          Consonant (Soft NAlveolar) : rest@(_ : _) ->
            -- Only when it's not the first letter
            Right $ Consonant (Soft NDental) : rest
          Consonant _ : _ ->
            Right str
          _ ->
            consonant $ Hard K

      '~' ->
        case str of
          Consonant (Hard K) : rest@(Consonant (Soft Ng) : _) ->
            Right rest
          _ ->
            Left UnexpectedTilde

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
            Left UnexpectedPulli

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

      ch -> Left $ UnknownChar ch
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
            Vowel _ : _ ->
              Left VowelHiatus
            _ ->
              Right $ Vowel v : str
        consonant c =
          Right $ Consonant c : str
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

toLatin :: TamilString -> String
toLatin (TamilString str) =
  case str of
    Consonant (Hard h) : rest@(Vowel _ : _) ->
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
    go acc str =
      case str of
        [] -> acc
        [Vowel v, Consonant (Hard TDental)] | v /= I Short, v /= I Long, v /= E Short, v /= E Long ->
          "th" ++ vowel v ++ acc
        Vowel v : rest ->
          go' rest $ vowel v
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
        Consonant (Hard h) : Consonant (Hard o) : rest ->
          go' rest [unvoiced o, unvoiced h]
        [Consonant (Hard h)] ->
          (++ acc) case h of
            Ch -> "s"
            _  -> [unvoiced h]
        Consonant (Hard h) : rest ->
          go' rest $ voiced h
        [Consonant (Soft NDental)] ->
          'n' : acc
        Consonant (Soft s) : rest ->
          go' rest $ soft s
        Consonant (Medium m) : rest ->
          go' rest case m of
            Y          -> "y"
            R          -> "r"
            LAlveolar  -> "l"
            V          -> "v"
            Zh         -> "zh"
            LRetroflex -> "L"
      where
        go' rest str = go (str ++ acc) rest

toTamil :: TamilString -> String
toTamil (TamilString str) =
  case go "" str of
    'ன' : rest -> 'ந' : rest
    other      -> other
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

normalize :: TamilString -> TamilString
normalize (TamilString str) =
  TamilString $ map go $
    case str of
      Vowel (U Short) : Consonant a : rest@(Consonant b : _) | a == b ->
        rest
      Vowel (U Short) : Consonant (Hard h) : Consonant (Soft s) : rest | s == getPaired h ->
        Consonant (Hard h) : rest
      Vowel (U Short) : Consonant (Hard K) : rest@(Vowel v : _) | not $ isShortVowel v ->
        rest
      Vowel (U Short) : rest ->
        rest
      _ ->
        str
  where
    go = \case
      Vowel (U _) -> Vowel $ U Short
      Vowel (O _) -> Vowel $ U Short
      Vowel _     -> Vowel $ A Short
      Consonant (Hard TRetroflex)   -> Consonant $ Hard TDental
      Consonant (Hard RAlveolar)    -> Consonant $ Medium R
      Consonant (Soft Ng)           -> Consonant $ Hard K
      Consonant (Soft Ny)           -> Consonant $ Hard Ch
      Consonant (Soft M)            -> Consonant $ Hard P
      Consonant (Soft _)            -> Consonant $ Hard TDental
      Consonant (Medium R)          -> Consonant $ Medium R
      Consonant (Medium LRetroflex) -> Consonant $ Medium LAlveolar
      other -> other

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

endsInLongVowel :: TamilString -> Bool
endsInLongVowel = \case
  TamilString (Vowel v : _) -> not $ isShortVowel v
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
        (Consonant _, _) ->
          suffix ++ root
        (_, Consonant c)
          | restIsShort ->
            middle c
          | otherwise ->
            suffix ++ root
        (Vowel (I Short), Vowel v) | not $ isShortVowel v ->
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

