{-# LANGUAGE TemplateHaskell #-}
module Biparse.Mixes.IOSpec where

import Biparse.Mixes.IO
import Language.Haskell.TH (doE, noBindS, varE, appTypeE)

spec :: Spec
spec = do
  describe "Easy" do
    let typedTests :: forall c ss.
          ( ConvertElement (Mixes c) (SubElement (SuperState c ss)) (AssociatedWriter ss) (RWST () (AssociatedWriter ss) () IO)
          , ConvertSequence (Mixes c) (SubState (SuperState c ss)) (AssociatedWriter ss) (RWST () (AssociatedWriter ss) () IO)
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
            it "Forward" $ fw one def "abc" >>= (`shouldBe` fromChar 'a')
            it "Backward" $ bw one (fromChar 'z') >>= (`shouldBe` "z")
          describe "naturalBaseTen" do
            it "Forward" $ fw naturalBaseTen def "123" >>= (`shouldBe` (123 :: Word8))
            it "Backward" $ bw naturalBaseTen (123 :: Word8) >>= (`shouldBe` "123")

    $(doE $ noBindS . (\(c,ss) -> varE "typedTests" `appTypeE` c `appTypeE` ss) <$> combinations contexts subStates)

