{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE QuantifiedConstraints #-}
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

word8 :: (One Word8 (p Word8), Profunctor p, forall u. Monad (p u)) => Iso p Word8
word8 = one
instance (One Word8 (p Word8), Profunctor p, forall u. Monad (p u)) => IsoClass (Bin Word8) p where iso = coerceIso word8

word16 :: (One Word8 (p Word8), Profunctor p, forall u. Monad (p u)) => Iso p Word16
word16 = wordTemplate 8 word8
instance (One Word8 (p Word8), Profunctor p, forall u. Monad (p u)) => IsoClass (Bin Word16) p where iso = coerceIso word16

word32 :: (One Word8 (p Word8), Profunctor p, forall u. Monad (p u)) => Iso p Word32
word32 = wordTemplate 16 word16
instance (One Word8 (p Word8), Profunctor p, forall u. Monad (p u)) => IsoClass (Bin Word32) p where iso = coerceIso word32

word64 :: (One Word8 (p Word8), Profunctor p, forall u. Monad (p u)) => Iso p Word64
word64 = wordTemplate 32 word32
instance (One Word8 (p Word8), Profunctor p, forall u. Monad (p u)) => IsoClass (Bin Word64) p where iso = coerceIso word64

wordTemplate :: forall p halfWord word.
  ( Bits word
  , Integral word
  , Integral halfWord
  , Profunctor p
  , Monad (p word)
  ) => Int -> Iso p halfWord -> Iso p word
wordTemplate shiftBy half = do
  h <- (`shiftL` shiftBy) . fromIntegral <$> half `upon` fromIntegral . (`shiftR` shiftBy)
  l <- fromIntegral <$> half `upon` fromIntegral
  return $ h .|. l

word128 ::
  ( One Word8 (p Word8)
  , Profunctor p
  , forall u. Monad (p u)
  ) => Iso p Word128
word128 = wideWordTemplate word64 word128Hi64 Word128
instance 
  ( One Word8 (p Word8)
  , Profunctor p
  , forall u. Monad (p u)
  ) => IsoClass (Bin Word128) p where iso = coerceIso word128

word256 ::
  ( One Word8 (p Word8)
  , Profunctor p
  , forall u. Monad (p u)
  ) => Iso p Word256
word256 = wideWordTemplate word128 (\(Word256 x y _ _) -> Word128 x y) \(Word128 word256hi word256m1) (Word128 word256m0 word256lo) -> Word256 {..}
instance
  ( One Word8 (p Word8)
  , Profunctor p
  , forall u. Monad (p u)
  ) => IsoClass (Bin Word256) p where
  iso = coerceIso word256

wideWordTemplate :: forall p halfWord word.
  ( Integral word
  , Num halfWord
  , Profunctor p
  , Monad (p word)
  ) => Iso p halfWord -> (word -> halfWord) -> (halfWord -> halfWord -> word) -> Iso p word
wideWordTemplate half getTopHalf combiner = do
  h <- half `upon` getTopHalf
  l <- half `upon` fromIntegral
  return $ h `combiner` l

-- * Ints

int8 :: forall p. (One Word8 (p Word8), Profunctor p, forall u. Monad (p u)) => Iso p Int8
int8 = unsafeCoerce $ word8 @p
instance (One Word8 (p Word8), Profunctor p, forall u. Monad (p u)) => IsoClass (Bin Int8) p where iso = coerceIso int8

int16 :: forall p. (One Word8 (p Word8), Profunctor p, forall u. Monad (p u)) => Iso p Int16
int16 = unsafeCoerce $ word16 @p
instance (One Word8 (p Word8), Profunctor p, forall u. Monad (p u)) => IsoClass (Bin Int16) p where iso = coerceIso int16

int32 :: forall p. (One Word8 (p Word8), Profunctor p, forall u. Monad (p u)) => Iso p Int32
int32 = unsafeCoerce $ word32 @p
instance (One Word8 (p Word8), Profunctor p, forall u. Monad (p u)) => IsoClass (Bin Int32) p where iso = coerceIso int32

int64 :: forall p. (One Word8 (p Word8), Profunctor p, forall u. Monad (p u)) => Iso p Int64
int64 = unsafeCoerce $ word64 @p
instance (One Word8 (p Word8), Profunctor p, forall u. Monad (p u)) => IsoClass (Bin Int64) p where iso = coerceIso int64

int128 :: forall p. (One Word8 (p Word8), Profunctor p, forall u. Monad (p u)) => Iso p Int128
int128 = unsafeCoerce $ word128 @p
instance (One Word8 (p Word8), Profunctor p, forall u. Monad (p u)) => IsoClass (Bin Int128) p where iso = coerceIso int128

