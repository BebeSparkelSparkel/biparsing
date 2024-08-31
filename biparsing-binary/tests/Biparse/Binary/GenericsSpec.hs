{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverloadedLists #-}
module Biparse.Binary.GenericsSpec where

--import Data.Word
import Biparse.Binary
import Biparse.Binary.Generics
--import Data.Bits

spec :: Spec
spec = runAllTests @() @() @() @() @() @() @TestSuite testSuite

type TestSuite :: (Type -> Type -> Type) -> Constraint
class TestSuite p where testSuite :: Proxy p -> Spec
instance
  ( ComapM p m
  , direction ~ WhichDirection (p ())
  , r ~ Read (p ())
  , s ~ State (p ())
  ) => TestSuite p where
  testSuite _ = describe (show $ typeRep @p) do
    let run' :: forall u v. Biparser p u v -> FilePath -> u -> String -> BaseMonad (p u) (StM' (p u) v)
        run' = run @p @r @s
    describe "genericBinaryAdtIsoClass" do
      let f = run' $ genericBinaryAdtIsoClass @ABC
      it "A" let
        fp = "genericBinaryAdtIsoClass-A.test"
        u = A
        in f fp A [0] `shouldReturn` makeResult @direction
          (IndexPosition fp 1)
          []
          [0]
          A
      it "B" let
        fp = "genericBinaryAdtIsoClass-B.test"
        bs = [1,5]
        x = B 5
        in f fp x bs `shouldReturn` makeResult @direction
          (IndexPosition fp 2)
          []
          bs
          x
      it "C" let
        fp = "genericBinaryAdtIsoClass-C.test"
        x = C 0x0102 0x03040506
        in f fp x [2,0,1,255,255,255,255,0] `shouldReturn` makeResult @direction
          (IndexPosition fp 7)
          [0]
          [2,1,2,3,4,5,6]
          x
--need to convert the new bidirecional tests (reference GeneralSpec.hs)
--biparsing-text/Prelude.hs needs to export a set of transformers that only use ByteString and ByteString Builders
--register with the Feds about the company ownership
--Haskell Planetarium feed aggragator https://haskell.pl-a.net/
--Anti Military Licenses https://ethicalsource.dev/licenses/

data ABC
  = A
  | B (Bin Word8)
  | C (Bin Word16) (Bin Word32)
  deriving (Show, Eq, Generic)

