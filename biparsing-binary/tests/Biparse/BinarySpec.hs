module Biparse.BinarySpec where

import Data.Bits
import Biparse.Binary
import Test.QuickCheck.Instances.ByteString ()
import Data.WideWord

default (Word8)

spec :: Spec
spec = do
  runAllTests @(ForwardProfunctors  'BinaryStrings () () ()) @ForwardTestSuite  forwardTestSuite
  runAllTests @(BackwardProfunctors 'BinaryStrings () () ()) @BackwardTestSuite backwardTestSuite

type ForwardTestSuite :: (Type -> Type -> Type) -> Constraint
class ForwardTestSuite p where forwardTestSuite :: Proxy p -> Spec
instance
  ( ComapM p m
  , Profunctor p
  , One Word8 (p Word8)
  , r ~ Read (p ())
  , s ~ State (p ())
  , forall u v. MakeForwardBinaryResultQ p u v
  , forall u. ShouldReturnQ p u
  , forall u a. Show a => ShowStM' (p u) a
  , forall u a. Eq a => EqStM' (p u) a
  , forall u. RunBase (TestParameters r s u [Word8]) (p u)
  , forall u. ConstructParameter u [Word8] (TestParameters r s u [Word8])
  , forall u. MonadFail (p u)
  , Typeable p
  , forall u. ShouldFailQ p u
  ) => ForwardTestSuite p where
  forwardTestSuite _ = describe (show $ typeRep @p) do

    test word8
    test word16
    test word32
    test word64
    test word128
    test word256
    test int8
    test int16
    test int32
    test int64
    test int128

    where
    test :: forall numType.
      ( Typeable numType
      , FiniteBits numType
      , Num numType
      , Show numType
      ) => Iso p numType -> Spec
    test bp = do
      prop typeString \bytes -> if olength bytes < byteSize
      then shouldFail $ f fp undefined bytes
      else f fp undefined bytes `shouldReturn` makeResult @'Forward
            (IndexPosition fp byteSize)
            (odrop byteSize bytes)
            (ofoldl' (\x y -> shiftL x 8 .|. fromIntegral y) zeroBits $ otake byteSize bytes :: numType)
      where
      f = run' bp
      fp = "binary-" <> typeString <> ".test"
      typeString = show $ typeRep @numType
      byteSize = finiteBitSize (zeroBits @numType) `div` 8

    run' :: forall u v. Biparser p u v -> FilePath -> u -> [Word8] -> BaseMonad (p u) (StM' (p u) v)
    run' = run @p @r @s

type BackwardTestSuite :: (Type -> Type -> Type) -> Constraint
class BackwardTestSuite p where backwardTestSuite :: Proxy p -> Spec
instance
  ( ComapM p m
  , Profunctor p
  , One Word8 (p Word8)
  , r ~ Read (p ())
  , s ~ State (p ())
  , forall u v. MakeBackwardBinaryResultQ p u v
  , forall u. ShouldReturnQ p u
  , forall u a. Show a => ShowStM' (p u) a
  , forall u a. Eq a => EqStM' (p u) a
  , forall u. RunBase (TestParameters r s u [Word8]) (p u)
  , forall u. ConstructParameter u [Word8] (TestParameters r s u [Word8])
  , forall u. MonadFail (p u)
  , Typeable p
  ) => BackwardTestSuite p where
  backwardTestSuite _ = describe (show $ typeRep @p) do

    test word8
    test word16
    test word32
    test word64
    test word128
    test word256
    test int8
    test int16
    test int32
    test int64
    test int128

    where
    test :: forall numType.
      ( Typeable numType
      , FiniteBits numType
      , Show numType
      , Arbitrary numType
      , Integral numType
      ) => Iso p numType -> Spec
    test bp = do
      prop typeString \binary -> f fp binary undefined `shouldReturn` makeResult @'Backward
        (toBytes binary :: [Word8])
        binary
      where
      f = run' bp
      fp = "binary-" <> typeString <> ".test"
      typeString = show $ typeRep @numType

    run' :: forall u v. Biparser p u v -> FilePath -> u -> [Word8] -> BaseMonad (p u) (StM' (p u) v)
    run' = run @p @r @s

toBytes :: (FiniteBits a, Integral a) => a -> [Word8]
toBytes x = fmap (fromIntegral . shiftR x) [n, n-8 .. 0]
  where n = finiteBitSize x - 8

instance Arbitrary Word128 where
  arbitrary = uncurry Word128 <$> arbitrary
  shrink = genericShrink
instance Arbitrary Word256 where
  arbitrary = arbitrary <&> to . coerce . from @(Word64,Word64,Word64,Word64)
  shrink = genericShrink
instance Arbitrary Int128 where
  arbitrary = uncurry Int128 <$> arbitrary
  shrink = genericShrink

class
  ( MakeResult 'Forward (IndexPosition FilePath -> [Word8] -> v -> StM' (p v) v)
  ) => MakeForwardBinaryResultQ p u v
instance
  ( MakeResult 'Forward (IndexPosition FilePath -> [Word8] -> v -> StM' (p v) v)
  ) => MakeForwardBinaryResultQ p u v
class
  ( MakeResult 'Backward ([Word8] -> u -> StM' (p u) u)
  ) => MakeBackwardBinaryResultQ p u v
instance
  ( MakeResult 'Backward ([Word8] -> u -> StM' (p u) u)
  ) => MakeBackwardBinaryResultQ p u v

