{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UndecidableInstances #-}
module Biparse.Binary
  ( Bin
  , word8
  , word16
  , word32
  , word64
  , word128
  , word256
  , int8
  , int16
  , int32
  , int64
  , int128
  ) where

newtype Bin a = Bin a deriving (Show, Eq, Ord, Num, Bits, Enum, Bounded)

-- * Words

word8 :: forall c m n a ss w. One c a m n ss Word8 w => Iso c m n a Word8
word8 = one
instance One c a m n ss Word8 w => IsoClass c m n a (Bin Word8) where iso = coerceIso word8

word16 :: forall c m n a ss w. One c a m n ss Word8 w => Iso c m n a Word16
word16 = wordTemplate 8 word8
instance One c a m n ss Word8 w => IsoClass c m n a (Bin Word16) where iso = coerceIso word16

word32 :: forall c m n a ss w. One c a m n ss Word8 w => Iso c m n a Word32
word32 = wordTemplate 16 word16
instance One c a m n ss Word8 w => IsoClass c m n a (Bin Word32) where iso = coerceIso word32

word64 :: forall c m n a ss w. One c a m n ss Word8 w => Iso c m n a Word64
word64 = wordTemplate 32 word32
instance One c a m n ss Word8 w => IsoClass c m n a (Bin Word64) where iso = coerceIso word64

wordTemplate :: forall c m n a halfWord word.
  ( Bits word
  , Monad m
  , Monad n
  , Integral word
  , Integral halfWord
  ) => Int -> Iso c m n a halfWord -> Iso c m n a word
wordTemplate shiftBy half = do
  h <- (`shiftL` shiftBy) . fromIntegral <$> half `upon` fromIntegral . (`shiftR` shiftBy)
  l <- fromIntegral <$> half `upon` fromIntegral
  return $ h .|. l

word128 :: forall c m n a ss w. One c a m n ss Word8 w => Iso c m n a Word128
word128 = wideWordTemplate word64 word128Hi64 Word128
instance One c a m n ss Word8 w => IsoClass c m n a (Bin Word128) where iso = coerceIso word128

word256 :: forall c m n a ss w. One c a m n ss Word8 w => Iso c m n a Word256
word256 = wideWordTemplate word128 (\(Word256 x y _ _) -> Word128 x y) \(Word128 word256hi word256m1) (Word128 word256m0 word256lo) -> Word256 {..}
instance One c a m n ss Word8 w => IsoClass c m n a (Bin Word256) where iso = coerceIso word256

wideWordTemplate :: forall c m n a halfWord word.
  ( Monad m
  , Monad n
  , Integral word
  , Num halfWord
  ) => Iso c m n a halfWord -> (word -> halfWord) -> (halfWord -> halfWord -> word) -> Iso c m n a word
wideWordTemplate half getTopHalf combiner = do
  h <- half `upon` getTopHalf
  l <- half `upon` fromIntegral
  return $ h `combiner` l

-- * Ints

int8 :: forall c m n a ss w. One c a m n ss Word8 w => Iso c m n a Int8
int8 = unsafeCoerce $ word8 @c @m @n @a @ss @w
instance One c a m n ss Word8 w => IsoClass c m n a (Bin Int8) where iso = coerceIso int8

int16 :: forall c m n a ss w. One c a m n ss Word8 w => Iso c m n a Int16
int16 = unsafeCoerce $ word16 @c @m @n @a @ss @w
instance One c a m n ss Word8 w => IsoClass c m n a (Bin Int16) where iso = coerceIso int16

int32 :: forall c m n a ss w. One c a m n ss Word8 w => Iso c m n a Int32
int32 = unsafeCoerce $ word32 @c @m @n @a @ss @w
instance One c a m n ss Word8 w => IsoClass c m n a (Bin Int32) where iso = coerceIso int32

int64 :: forall c m n a ss w. One c a m n ss Word8 w => Iso c m n a Int64
int64 = unsafeCoerce $ word64 @c @m @n @a @ss @w
instance One c a m n ss Word8 w => IsoClass c m n a (Bin Int64) where iso = coerceIso int64

int128 :: forall c m n a ss w. One c a m n ss Word8 w => Iso c m n a Int128
int128 = unsafeCoerce $ word128 @c @m @n @a @ss @w
instance One c a m n ss Word8 w => IsoClass c m n a (Bin Int128) where iso = coerceIso int128

