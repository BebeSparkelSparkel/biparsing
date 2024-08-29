{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# OPTIONS_GHC -Wno-orphans #-}
#include "MachDeps.h"
module Biparse.Text.Numeric (
naturalBaseTen,
intBaseTen,
--eNotation,
--realBaseTen,
hex,
CharCase(..),
digitsHexList,
capitalHexList,
lowerHexList,
) where

import Biparse.General (takeTri)
import Data.Bits (Bits, zeroBits, shift, shiftR, (.&.))
import Data.Int (Int8, Int16, Int32, Int64)
import Data.Tuple (swap)
import Data.Word (Word, Word8, Word16, Word32, Word64)
--import GHC.Float (Float, Double)
import GHC.Num ((*), (-), negate, abs)
import GHC.Real (Integral, fromIntegral, div, mod)
import Numeric (showHex)
import Data.Ix (Ix, index, inRange)
import Data.List (lookup)
import GHC.Num (Num((+)))
import Numeric.Natural (Natural)
import Biparse.IsoClass (IsoClass(iso))
import Biparse.Core.Classes (Diverge(diverge), UnfoldlExactN(unfoldlExactN))
import Data.Tuple.Extra ((&&&))

naturalBaseTen :: forall p f b number char. NaturalBaseTen p f b number char => Iso p number
naturalBaseTen = naturalLimitedBaseTen maxBound
type NaturalBaseTen p f b number char =
  ( Diverge (p number) f b number
  , MonadFail f
  , Try f
  , Alternative f
  , UnfoldlExactN b char
  , Monad b
  , Show number
  , Integral number
  , Bounded number
  , NumberOfDigits number
  , char ~ Item' p
  , Show char
  , Ix char
  , Enum char
  , IsChar char
  , One char f
  )
instance NaturalBaseTen p f b Word   char => IsoClass p Word   where iso = naturalBaseTen
instance NaturalBaseTen p f b Word8  char => IsoClass p Word8  where iso = naturalBaseTen
instance NaturalBaseTen p f b Word16 char => IsoClass p Word16 where iso = naturalBaseTen
instance NaturalBaseTen p f b Word32 char => IsoClass p Word32 where iso = naturalBaseTen
instance NaturalBaseTen p f b Word64 char => IsoClass p Word64 where iso = naturalBaseTen

naturalLimitedBaseTen :: forall p f b number char.
  ( Diverge (p number) f b number
  , MonadFail f
  , Try f
  , Alternative f
  , UnfoldlExactN b char
  , Monad b
  , Show number
  , Integral number
  , NumberOfDigits number
  , char ~ Item' p
  , Show char
  , Ix char
  , Enum char
  , IsChar char
  , One char f
  )
  => number
  -> Iso p number
naturalLimitedBaseTen limit = diverge @(p number) @f @b @number
  do
    x <- getDigit
    fw x
  \n -> do
    _ <- unfoldlExactN (numDigits n) ((`div` 10) &&& toEnum . (+ fromEnum zero) . fromIntegral . (`mod` 10)) n
    pure n
  where
  fw x = Just <$> getDigit <|> pure Nothing >>= maybe (pure x) \d -> if limit - x > d
    then fw $ 10 * x + d
    else fail $ "Exceeded limit of " <> show limit
  getDigit = try do
    c <- one
    if inRange range c
    then pure $ fromIntegral $ index range c
    else fail $ show c <> " is not a digit."
  range = (zero, fromChar '9')
  zero :: Item' p
  zero = fromChar '0'

intBaseTen :: forall p f b m number char. IntBaseTen p f b m number char => Iso p number
intBaseTen = do
  s <- sign @char
  n <- naturalBaseTen `upon` abs
  pure $ s n
type IntBaseTen p f b m number char =
  ( Sign p m number char
  , NaturalBaseTen p f b number char
  , Monad (p number)
  )
instance IntBaseTen p f b m Int   char => IsoClass p Int   where iso = intBaseTen
instance IntBaseTen p f b m Int8  char => IsoClass p Int8  where iso = intBaseTen
instance IntBaseTen p f b m Int16 char => IsoClass p Int16 where iso = intBaseTen
instance IntBaseTen p f b m Int32 char => IsoClass p Int32 where iso = intBaseTen
instance IntBaseTen p f b m Int64 char => IsoClass p Int64 where iso = intBaseTen

---- | Only wirtes digits and not powers of 10.
--eNotation :: forall p f b m number char.
--  ( Diverge (p Int) f b Int
--  , forall u. MonadFail (p u)
--  , forall u. Alternative (p u)
--  , forall u. Try (p u)
--  , Profunctor p
--  , One p
--  , ComapM p m
--  , MonadFail m
--  , MonadFail f
--  , Try f
--  , Alternative f
--  , UnfoldlExactN b char
--  , Monad b
--  , Fractional number
--  , char ~ Item' p
--  , Show char
--  , Ix char
--  , Enum char
--  , IsChar char
--  ) => Iso p number
--eNotation = do
--  digits <- realBaseTen
--  power :: Maybe Int <- comap (const Nothing) $ optional do
--    take (fromChar 'E') <|> take (fromChar 'e')
--    intBaseTen
--  pure $ maybe id ((*) . (10 ^^)) power $ digits
--
--realBaseTen :: forall p number.
--  ()
--  => Iso p number
--realBaseTen =
--  try do
--    s <- sign
--    ws <- digitsBaseTen `upon` abs
--    ds <- comap (const mempty) $ ignoreBackwardIso
--      $   try (cons <$> (fromChar '.' <$ take (fromChar '.')) <*> digitsBaseTen)
--      <|> pure mempty
--    maybe (fail "Could not read a realBaseTen.") (pure . s) $ readMay $ fmap toChar $ toList $ ws <> ds
--  <|> do
--    --cs <- peek $ Data.Sequences.take 20 <$> rest `upon` const mempty
--    --fail $ "Could not parse " <> show cs <> " to a base 10 real."
--    fail $ "Could not parse a base 10 real."
--instance () => IsoClass p Float  where iso = realBaseTen
--instance () => IsoClass p Double where iso = realBaseTen

class NumberOfDigits number where numDigits :: number -> Int
#if WORD_SIZE_IN_BITS == 64
instance NumberOfDigits Word   where numDigits = numDigitsWord64
instance NumberOfDigits Int    where numDigits = numDigitsWord64 @Word64 . intToWord
#elif WORD_SIZE_IN_BITS == 32
instance NumberOfDigits Word   where numDigits = numDigitsWord32
instance NumberOfDigits Int    where numDigits = numDigitsWord32 @Word32 . intToWord
#endif
instance NumberOfDigits Word8  where numDigits = numDigitsWord8
instance NumberOfDigits Word16 where numDigits = numDigitsWord16
instance NumberOfDigits Word32 where numDigits = numDigitsWord32
instance NumberOfDigits Word64 where numDigits = numDigitsWord64
instance NumberOfDigits Int8   where numDigits = numDigitsWord8  @Word8  . intToWord
instance NumberOfDigits Int16  where numDigits = numDigitsWord16 @Word16 . intToWord
instance NumberOfDigits Int32  where numDigits = numDigitsWord32 @Word32 . intToWord
instance NumberOfDigits Int64  where numDigits = numDigitsInt64  @Word64 . intToWord
intToWord :: (Integral a, Num b) => a -> b
intToWord = fromIntegral . abs
numDigitsWord64 :: forall a. (Num a, Ord a) => a -> Int
numDigitsWord64 x = if
  | x >= 10000000000000000000 -> 20
  | otherwise -> numDigitsInt64 x
numDigitsInt64 :: forall a. (Num a, Ord a) => a -> Int
numDigitsInt64 x = if
  | x >= 1000000000000000000 -> 19
  | x >= 100000000000000000 -> 18
  | x >= 10000000000000000 -> 17
  | x >= 1000000000000000 -> 16
  | x >= 100000000000000 -> 15
  | x >= 10000000000000 -> 14
  | x >= 1000000000000 -> 13
  | x >= 100000000000 -> 12
  | x >= 10000000000 -> 11
  | otherwise -> numDigitsWord32 x
numDigitsWord32 :: forall a. (Num a, Ord a) => a -> Int
numDigitsWord32 x = if
  | x >= 1000000000 -> 10
  | x >= 100000000 -> 9
  | x >= 10000000 -> 8
  | x >= 1000000 -> 7
  | x >= 100000 -> 6
  | otherwise -> numDigitsWord16 x
numDigitsWord16 :: forall a. (Num a, Ord a) => a -> Int
numDigitsWord16 x = if
  | x >= 10000 -> 5
  | x >= 1000 -> 4
  | otherwise -> numDigitsWord8 x
numDigitsWord8 :: forall a. (Num a, Ord a) => a -> Int
numDigitsWord8 x = if
  | x >= 100 -> 3
  | x >= 10 -> 2
  | otherwise -> 1

sign :: forall char p m number. Sign p m number char => Biparser p number (number -> number)
sign = comap (< 0) $ takeTri (fromChar '-' :: char) True negate <|> pure id
type Sign p m number char =
  ( MonadFail (p Bool)
  , Alternative (p Bool)
  , Try (p Bool)
  , Profunctor p
  , One char (p char)
  , ComapM p m
  , MonadFail m
  , Ord number
  , Num number
  , IsChar char
  , Eq char
  , Show char
  )

-- | Consume n hex characters lower or upper case. Print n hex characters with a case decided by 'charCase'.
hex :: forall (charCase :: CharCase) p m number char.
  ( forall u. MonadFail (p u)
  , ComapM p m
  , MonadFail m
  , Bits number
  , Integral number
  , Show number
  , HexCharMap charCase
  , Ord char
  , IsChar char
  , Show char
  , One char (p char)
  )
  => Natural
  -> Iso p number
hex = hex' . fromEnum
  where
  hex' = \case
    0 -> pure zeroBits
    (pred -> n) ->  do
      c <- one @char @(p char) `uponM` lookupChar @charCase . (.&. f) . (`shiftR` (4 * n))
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

