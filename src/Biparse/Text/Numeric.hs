{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Biparse.Text.Numeric
  ( NaturalConstraints
  , naturalBaseTen
  , naturalBaseTen'
  , IntConstraints
  , intBaseTen
  , scientific
  , RealConstrints
  , realBaseTen
  , hex
  , CharCase(..)
  , digitsHexList
  , capitalHexList
  , lowerHexList
  ) where

import Biparse.Biparser (Iso, IsoClass(iso), split, upon, SubState, SubStateContext, try, ignoreBackward, split, Biparser, ElementContext, comap, uponM, one, SubElement, GetSubState, UpdateStateWithElement, peek)
import Biparse.General (take, optional, takeTri, rest)
import Biparse.Text (CharElement)
import Data.Bits (Bits, zeroBits, shift, shiftR, (.&.))
import Data.Char (isDigit)
import Data.Int (Int8, Int16, Int32, Int64)
import Data.Map.Strict qualified as M
import Data.Tuple (swap)
import Data.Word (Word, Word8, Word16, Word32, Word64)
import GHC.Float (Float, Double)
import GHC.Num ((*), negate, Integer, abs)
import GHC.Real (Fractional, (^^))
import GHC.Real (Integral)
import Numeric (showHex)
import Safe (readMay)
import Type.Reflection (Typeable, typeRep)
import Data.Sequences qualified


instance NaturalConstraints c s m n Word   char => IsoClass c m n s Word   where iso = naturalBaseTen
instance NaturalConstraints c s m n Word8  char => IsoClass c m n s Word8  where iso = naturalBaseTen
instance NaturalConstraints c s m n Word16 char => IsoClass c m n s Word16 where iso = naturalBaseTen
instance NaturalConstraints c s m n Word32 char => IsoClass c m n s Word32 where iso = naturalBaseTen
instance NaturalConstraints c s m n Word64 char => IsoClass c m n s Word64 where iso = naturalBaseTen

type NaturalConstraints c s m n number char =
  ( CharElement s char
  , IsSequence (SubState s)
  , MonadState s m
  , MonadFail m
  , MonadWriter (SubState s) n
  , MonadFail n
  , SubStateContext c s
  , Read number
  , Num number
  , Show number
  , Typeable number
  , Show (SubState s)
  )

naturalBaseTen :: forall c s m n number char. NaturalConstraints c s m n number char => Iso c m n s number
naturalBaseTen = do
  ds <- fmap toChar . toList <$> digitsBaseTen `upon` abs
  maybe
    do
      cs <- peek $ Data.Sequences.take 20 <$> rest `upon` const mempty
      fail $ "Could not parse " <> show cs <> " to " <> show (typeRep @number) <> " base 10."
    pure
    $ readMay ds

naturalBaseTen' :: forall number c s m n char. NaturalConstraints c s m n number char => Iso c m n s number
naturalBaseTen' = naturalBaseTen

instance IntConstraints c s m n Int   char => IsoClass c m n s Int   where iso = intBaseTen
instance IntConstraints c s m n Int8  char => IsoClass c m n s Int8  where iso = intBaseTen
instance IntConstraints c s m n Int16 char => IsoClass c m n s Int16 where iso = intBaseTen
instance IntConstraints c s m n Int32 char => IsoClass c m n s Int32 where iso = intBaseTen
instance IntConstraints c s m n Int64 char => IsoClass c m n s Int64 where iso = intBaseTen

type IntConstraints c s m n number char =
  ( NaturalConstraints c s m n number char
  , MonadPlus m
  , Alternative n
  , Num number
  , Ord number
  , ElementContext c s
  )

intBaseTen :: forall c s m n number char. IntConstraints c s m n number char => Iso c m n s number
intBaseTen = do
  s <- sign
  n <- naturalBaseTen
  pure $ s n

type RealConstrints c s m n number ss char =
  ( NaturalConstraints c s m n number char
  , MonadPlus m
  , ElementContext c s
  , Show ss
  , Alternative n
  , Num number
  , Ord number
  , ss ~ SubState s
  )

instance RealConstrints c s m n Float  ss char => IsoClass c m n s Float  where iso = realBaseTen
instance RealConstrints c s m n Double ss char => IsoClass c m n s Double where iso = realBaseTen

-- | Only wirtes digits and not powers of 10.
scientific :: forall c s m n number ss char.
  ( Fractional number
  , RealConstrints c s m n number ss char
  ) => Iso c m n s number
scientific = do
  digits <- realBaseTen
  power :: Maybe Integer <- comap (const Nothing) $ optional do
    take (fromChar 'E') <|> take (fromChar 'e')
    intBaseTen
  pure $ maybe id ((*) . (10 ^^)) power $ digits

realBaseTen :: forall c s m n number ss char.
  ( RealConstrints c s m n number ss char
  ) => Iso c m n s number
realBaseTen = do
  s <- sign
  ws <- digitsBaseTen `upon` abs
  ds <- comap (const mempty) $ ignoreBackward
    $   try (cons <$> (fromChar '.' <$ take (fromChar '.'))  <*> digitsBaseTen)
    <|> pure mempty
  maybe (fail "Could not parse real base 10.") (pure . s) $ readMay $ fmap toChar $ toList $ ws <> ds

digitsBaseTen :: forall c m n s u ss char.
  ( CharElement s char
  , Show u
  , IsSequence ss
  , MonadState s m
  , MonadWriter ss n
  , SubStateContext c s
  , ss ~ SubState s
  ) => Biparser c s m n u ss
digitsBaseTen = split (state $ span $ isDigit . toChar) `upon` fromList . fmap fromChar . show

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

sign :: forall c s m n number ss char.
  ( CharElement s char
  , MonadState s m
  , MonadFail m
  , MonadPlus m
  , IsSequence ss
  , MonadWriter ss n
  , MonadFail n
  , Alternative n
  , Num number
  , Ord number
  , ElementContext c s
  , ss ~ SubState s
  ) => Biparser c s m n number (number -> number)
sign = comap deduceSign $ takeTri (fromChar '-') Negative negate <|> pure id

-- | Consume n hex characters lower or upper case. Print n hex characters with a case decided by 'charCase'.
hex :: forall (charCase :: CharCase) c m n a number text char.
  -- m
  ( MonadState a m
  , MonadFail m
  , Alternative m
  -- n
  , MonadWriter text n
  , MonadFail n
  -- text
  , IsSequence text
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
  $ lookupMap (digitsHexList <> capitalHexList <> lowerHexList) c

data CharCase = UpperCase | LowerCase
type HexCharMap :: CharCase -> Constraint
class HexCharMap charCase where lookupChar :: (MonadFail m, CN char number, Integral number, Show number) => number -> m char
instance HexCharMap 'UpperCase where lookupChar = lookupChar' capitalHexList
instance HexCharMap 'LowerCase where lookupChar = lookupChar' lowerHexList

lookupChar' :: (MonadFail m, IsChar char, Integral number, Show number) => [(char,number)] -> number -> m char
lookupChar' chs x
  = maybe (fail $ "Could not convert 0x" <> showHex x " to an hex digit.") pure
  $ lookupMap (fmap swap $ digitsHexList <> chs) x

lookupMap :: Ord a => [(a,b)] -> a -> Maybe b
lookupMap = (M.!?) . M.fromList

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

