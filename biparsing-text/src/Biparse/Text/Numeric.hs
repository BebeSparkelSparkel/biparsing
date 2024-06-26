{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Biparse.Text.Numeric
  ( NaturalConstraints
  , naturalBaseTen
  , charToDigitMay
  , charToDigit
  , naturalBaseTen'
  , IntConstraints
  , intBaseTen
  , eNotation
  , RealConstrints
  , realBaseTen
  , hex
  , CharCase(..)
  , digitsHexList
  , capitalHexList
  , lowerHexList
  ) where

import Biparse.General (take, optional, takeTri, rest)
import Biparse.Text (CharElement)
import Data.Bits (Bits, zeroBits, shift, shiftR, (.&.))
import Data.Char (isDigit)
import Data.Int (Int8, Int16, Int32, Int64, Int)
import Data.Tuple (swap)
import Data.Word (Word, Word8, Word16, Word32, Word64)
import GHC.Float (Float, Double)
import GHC.Num ((*), negate, Integer, abs)
import GHC.Real (Fractional, (^^), Integral)
import Numeric (showHex)
import Safe (readMay, indexMay)
import Data.Sequences qualified
import Data.Ix (Ix, index)
import Data.List (lookup)
import GHC.Num (Num((+)))
import Text.Read (Read)
import Numeric.Natural (Natural)
import Data.Maybe (Maybe(Nothing))

instance NaturalConstraints c s m n Word   text char w => IsoClass c m n s Word   where iso = naturalBaseTen
instance NaturalConstraints c s m n Word8  text char w => IsoClass c m n s Word8  where iso = naturalBaseTen
instance NaturalConstraints c s m n Word16 text char w => IsoClass c m n s Word16 where iso = naturalBaseTen
instance NaturalConstraints c s m n Word32 text char w => IsoClass c m n s Word32 where iso = naturalBaseTen
instance NaturalConstraints c s m n Word64 text char w => IsoClass c m n s Word64 where iso = naturalBaseTen

type NaturalConstraints c s m n number text char w =
  ( CharElement s char
  , Ix char
  , IsSequence text
  , MonadState s m
  , MonadFail m
  , MonadWriter w n
  , MonadFail n
  , SubStateContext c s
  , Enum number
  , Show number
  , Show text
  , ConvertSequence c text w n
  , ContextualStateTransformerPLEASEREMOVESUFFIX c text m
  , text ~ SubState s
  )

naturalBaseTen :: forall c s m n number text char w.
  NaturalConstraints c s m n number text char w
  => Iso c m n s number
naturalBaseTen = do
  ds <- digitsBaseTen
  if null ds
  then do
    cs <- peek $ Data.Sequences.take 20 <$> rest `upon` const mempty
    fail $ "Could not parse " <> show cs <> " to a base 10 natural."
  else pure $ toEnum $ foldl' (\x c -> x * 10 + charToDigit c) 0 ds

charToDigitMay :: (Ix char, IsChar char) => char -> Maybe Int
charToDigitMay = indexMay digitRange

charToDigit :: (Ix char, IsChar char) => char -> Int
charToDigit = index digitRange

digitRange :: IsChar char => (char, char)
digitRange = (fromChar '0', fromChar '9')

naturalBaseTen' :: forall number c s m n text char w.
  NaturalConstraints c s m n number text char w
  => Iso c m n s number
naturalBaseTen' = naturalBaseTen

instance IntConstraints c s m n Int   text char w e => IsoClass c m n s Int   where iso = intBaseTen
instance IntConstraints c s m n Int8  text char w e => IsoClass c m n s Int8  where iso = intBaseTen
instance IntConstraints c s m n Int16 text char w e => IsoClass c m n s Int16 where iso = intBaseTen
instance IntConstraints c s m n Int32 text char w e => IsoClass c m n s Int32 where iso = intBaseTen
instance IntConstraints c s m n Int64 text char w e => IsoClass c m n s Int64 where iso = intBaseTen

type IntConstraints c s m n number text char w e =
  ( NaturalConstraints c s m n number text char w
  -- m
  , MonadError e m
  , Alt m
  -- n
  , Alt n
  -- number
  , Num number
  , Ord number
  , Show number
  -- context
  , ElementContext c s
  -- w
  , ConvertSequence c text w n
  , ConvertElement c char w n
  )

intBaseTen :: forall c s m n number text char w e.
  IntConstraints c s m n number text char w e
  => Iso c m n s number
intBaseTen = do
  s <- sign
  n <- naturalBaseTen `upon` abs
  pure $ s n

type RealConstrints c s m n number text char w e =
  ( NaturalConstraints c s m n number text char w
  -- m
  , Alt m
  , MonadError e m
  -- n
  , Alt n
  -- number
  , Num number
  , Ord number
  , Show number
  , Read number
  -- context
  , ElementContext c s
  , ContextualStateTransformerPLEASEREMOVESUFFIX c text m
  -- text
  , Show text
  , text ~ SubState s
  -- w
  , ConvertSequence c text w n
  , ConvertElement c char w n
  )

instance RealConstrints c s m n Float  text char w e => IsoClass c m n s Float  where iso = realBaseTen
instance RealConstrints c s m n Double text char w e => IsoClass c m n s Double where iso = realBaseTen

-- | Only wirtes digits and not powers of 10.
eNotation :: forall c s m n number text char w e.
  ( RealConstrints c s m n number text char w e
  , Fractional number
  ) => Iso c m n s number
eNotation = do
  digits <- realBaseTen
  power :: Maybe Integer <- comap (const Nothing) $ optional do
    take (fromChar 'E') <!> take (fromChar 'e')
    intBaseTen
  pure $ maybe id ((*) . (10 ^^)) power $ digits

realBaseTen :: forall c s m n number text char w e.
  RealConstrints c s m n number text char w e
  => Iso c m n s number
realBaseTen =
  try do
    s <- sign
    ws <- digitsBaseTen `upon` abs
    ds <- comap (const mempty) $ ignoreBackwardIso
      $   try (cons <$> (fromChar '.' <$ take (fromChar '.')) <*> digitsBaseTen)
      <!> pure mempty
    maybe (fail "Could not read a realBaseTen.") (pure . s) $ readMay $ fmap toChar $ toList $ ws <> ds
  <!> do
    cs <- peek $ Data.Sequences.take 20 <$> rest `upon` const mempty
    fail $ "Could not parse " <> show cs <> " to a base 10 real."

-- DEV NOTE: show should not be used
digitsBaseTen :: forall c m n s u text char w.
  ( CharElement s char
  , Show u
  , IsSequence text
  , MonadState s m
  , MonadWriter w n
  , ConvertSequence c text w n
  , SubStateContext c s
  , ContextualStateTransformerPLEASEREMOVESUFFIX c text m
  , text ~ SubState s
  ) => Biparser c s m n u text
digitsBaseTen = split (state @c $ span $ isDigit . toChar) `upon` fromList . fmap fromChar . show

data Sign = Positive | Negative | Zero deriving (Show, Eq)
deduceSign :: forall number.
  ( Ord number
  , Num number
  )
  => number
  -> Sign
deduceSign x
  | x < 0 = Negative
  | x > 0 = Positive
  | otherwise = Zero

sign :: forall c s m n number text char w e.
  ( MonadState s m
  , MonadFail m
  , Alt m
  , MonadError e m
  -- n
  , MonadWriter w n
  , MonadFail n
  , Alt n
  -- number
  , Num number
  , Ord number
  -- text
  , IsSequence text
  , CharElement s char
  -- w
  , ConvertElement c char w n
  -- context
  , ElementContext c s
  -- assignments
  , text ~ SubState s
  ) => Biparser c s m n number (number -> number)
sign = comap deduceSign $ takeTri (fromChar '-') Negative negate <!> pure id

-- | Consume n hex characters lower or upper case. Print n hex characters with a case decided by 'charCase'.
hex :: forall (charCase :: CharCase) c m n a number text char w.
  -- m
  ( MonadState a m
  , MonadFail m
  , Alt m
  -- n
  , MonadWriter w n
  , MonadFail n
  -- text
  , IsSequence text
  -- w
  , ConvertElement c char w n
  -- char
  , CharElement a char
  -- number
  , Bits number
  , Integral number
  , Show number
  -- context
  , GetSubState a
  , UpdateStateWithElement c a
  , HexCharMap charCase
  -- assignments
  , text ~ SubState a
  , char ~ SubElement a
  )
  => Natural
  -> Iso c m n a number
hex = hex' . fromEnum
  where
  hex' = \case
    0 -> pure zeroBits
    (pred -> n) ->  do
      c <- one `uponM` lookupChar @charCase . (.&. f) . (`shiftR` (4 * n))
      h <- lookupHex c
      (shift h (4 * n) +) <$> hex' n 
  f :: number
  f = 0xf

lookupHex :: (IsChar char, Ord char, Show char, Num number, MonadFail m) => char -> m number
lookupHex c
  = maybe (fail $ "Could not convert " <> show c <> " to hex value.") pure
  $ lookup c
  $ digitsHexList <> capitalHexList <> lowerHexList

data CharCase = UpperCase | LowerCase
type HexCharMap :: CharCase -> Constraint
class HexCharMap charCase where lookupChar :: (MonadFail m, CN char number, Integral number, Show number) => number -> m char
instance HexCharMap 'UpperCase where lookupChar = lookupChar' capitalHexList
instance HexCharMap 'LowerCase where lookupChar = lookupChar' lowerHexList

lookupChar' :: (MonadFail m, IsChar char, Integral number, Show number) => [(char,number)] -> number -> m char
lookupChar' chs x
  = maybe (fail $ "Could not convert 0x" <> showHex x " to an hex digit.") pure
  $ lookup x
  $ fmap swap $ digitsHexList <> chs

type CN char number =
  ( IsChar char
  , Num number
  )
digitsHexList :: CN char number => [(char,number)]
digitsHexList = fmap (first fromChar) 
  [ ('0', 0x0)
  , ('1', 0x1)
  , ('2', 0x2)
  , ('3', 0x3)
  , ('4', 0x4)
  , ('5', 0x5)
  , ('6', 0x6)
  , ('7', 0x7)
  , ('8', 0x8)
  , ('9', 0x9)
  ]
capitalHexList :: CN char number => [(char,number)]
capitalHexList = fmap (first fromChar) 
  [ ('A', 0xA)
  , ('B', 0xB)
  , ('C', 0xC)
  , ('D', 0xD)
  , ('E', 0xE)
  , ('F', 0xF)
  ]
lowerHexList :: CN char number => [(char,number)]
lowerHexList = fmap (first fromChar) 
  [ ('a', 0xa)
  , ('b', 0xb)
  , ('c', 0xc)
  , ('d', 0xd)
  , ('e', 0xe)
  , ('f', 0xf)
  ]

