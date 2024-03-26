{-# LANGUAGE TemplateHaskell #-}
module Biparse.Mixes.EitherSpec where

import Biparse.Mixes.Either
import Language.Haskell.TH (doE, noBindS, varE, appTypeE)

spec :: Spec
spec = do
  describe "Easy" do
    let typedTests :: forall c ss.
          ( ChangeMonad StringErrorIS (FM c ss) (Either String)
          , ConvertElement (Mixes c) (SubElement (SuperState c ss)) (AssociatedWriter ss) (RWST () (AssociatedWriter ss) () EitherString)
          , ConvertSequence (Mixes c) (SubState (SuperState c ss)) (AssociatedWriter ss) (RWST () (AssociatedWriter ss) () EitherString)
          , GetSubState (SuperState c ss)
          , IsSequence (SubState (SuperState c ss))
          , IsString ss
          , Monoid (AssociatedWriter ss)
          , IsString (AssociatedWriter ss)
          , Eq (AssociatedWriter ss)
          , Show (AssociatedWriter ss)
          , InitSuperState c ss
          , Show (SubElement (SuperState c ss))
          , IsChar (SubElement (SuperState c ss))
          , Ix (SubElement (SuperState c ss))
          , Show (SubState (SuperState c ss))
          , Default (SuperArg (SuperState c ss))
          ) => Spec
        typedTests = do
          let fw = decodeEasy @c @ss
              bw = encodeEasy @c @ss
          describe "one" do
            it "Forward" $ fw one def "abc" `shouldBe` Right (fromChar 'a')
            it "Backward" $ bw one (fromChar 'z') `shouldBe` Right "z"
          describe "naturalBaseTen" do
            it "Forward" $ fw naturalBaseTen def "123" `shouldBe` Right (123 :: Word8)
            it "Backward" $ bw naturalBaseTen (123 :: Word8) `shouldBe` Right "123"

    $(doE $ noBindS . (\(c,ss) -> varE "typedTests" `appTypeE` c `appTypeE` ss) <$> combinations contexts subStates)

