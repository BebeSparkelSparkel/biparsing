{-# LANGUAGE DataKinds #-}
module Biparse.AlternativeAttributesSpec where

import Biparse.AlternativeAttributes

spec :: Spec
spec = do
  describe "runAtt" do
    let bp :: Iso () IO IO () String () (Identity String) Char
        bp = runAtt @'[Bool] $ a @Bool one <!>> failAtt

    describe "forward" do
      let f = runForward @() bp

      it "consumes one" $ f "abc" >>= (`shouldBe` ('a',"bc"))

      it "fails" $ f mempty `shouldThrow` isUserError

  describe "totalAtt" do
    let bp :: Iso () IO IO () String () (Identity String) Char
        bp = runAtt @'[ 'True, 'False] $
          (a @'True one <!>> failAtt) `totalAtt`
          a @'False \x -> x <!> pure 'x'

    describe "forward" do
      let f = runForward @() bp

      it "consumes one" $ f "abc" >>= (`shouldBe` ('a',"bc"))

      it "gives alternative" $ f mempty >>= (`shouldBe` ('x',mempty))

