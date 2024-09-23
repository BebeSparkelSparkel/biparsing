{-# OPTIONS_GHC -Wno-type-defaults #-}
module Biparse.Binary.GenericsSpec where

import Biparse.Binary
import Biparse.Binary.Generics

default (Word8)

spec :: Spec
spec = runAllTests @(Profunctors 'BinaryStrings () () () () () ()) @TestSuite testSuite

type TestSuite :: (Type -> Type -> Type) -> Constraint
class TestSuite p where testSuite :: Proxy p -> Spec
instance
  ( ComapM p m
  , Profunctor p
  , One Word8 (p Word8)
  , MonadFail m
  , direction ~ WhichDirection (p ())
  , r ~ Read (p ())
  , s ~ State (p ())
  , forall u v. MakeBinaryResultQ direction p u v
  , forall u. ShouldReturnQ p u
  , forall u a. Show a => ShowStM' (p u) a
  , forall u a. Eq a => EqStM' (p u) a
  , forall u. RunBase (TestParameters r s u [Word8]) (p u)
  , forall u. ConstructParameter u [Word8] (TestParameters r s u [Word8])
  , forall u. Try (p u)
  , forall u. Alternative (p u)
  , forall u. MonadFail (p u)
  , Typeable p
  ) => TestSuite p where
  testSuite _ = describe (show $ typeRep @p) do
    let run' :: forall u v. Biparser p u v -> FilePath -> u -> [Word8] -> BaseMonad (p u) (StM' (p u) v)
        run' = run @p @r @s

    describe "genericBinaryAdtIsoClass" do
      let f = run' $ genericBinaryAdtIsoClass @ABC
      it "A" let
        fp = "genericBinaryAdtIsoClass-A.test"
        in f fp A [0] `shouldReturn` makeResult @direction
          (IndexPosition fp 1)
          ([] :: [Word8])
          [0]
          A
      it "B" let
        fp = "genericBinaryAdtIsoClass-B.test"
        bs :: (IsList binary, Item binary ~ Word8) => binary
        bs = fromList [1,5]
        x = B 5
        in f fp x bs `shouldReturn` makeResult @direction
          (IndexPosition fp 2)
          ([] :: [Word8])
          (bs :: [Word8])
          x
      it "C" let
        fp = "genericBinaryAdtIsoClass-C.test"
        x = C 0x0102 0x03040506
        bytes, remainder :: [Word8]
        bytes = [2,1,2,3,4,5,6]
        remainder = [0]
        in f fp x (fromList $ bytes <> remainder) `shouldReturn` makeResult @direction
          (IndexPosition fp $ olength bytes)
          remainder
          bytes
          x

data ABC
  = A
  | B (Bin Word8)
  | C (Bin Word16) (Bin Word32)
  deriving (Show, Eq, Generic)

class
  ( MakeResult d (IndexPosition FilePath -> [Word8] -> [Word8] -> v -> StM' (p u) v)
  ) => MakeBinaryResultQ d p u v
instance
  ( MakeResult d (IndexPosition FilePath -> [Word8] -> [Word8] -> v -> StM' (p u) v)
  ) => MakeBinaryResultQ d p u v

