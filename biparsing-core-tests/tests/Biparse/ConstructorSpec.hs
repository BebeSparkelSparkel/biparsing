{-# LANGUAGE UndecidableInstances #-}
module Biparse.ConstructorSpec where

import Biparse.Constructor

spec :: Spec
spec = do
  describe "focusOne" do
    fb "one"
      (focusOne @Lift @EitherString headAlt do
        expect _1 1
        x <- exposes (^. _2)
        return $ succ x
      :: Biparser () (Identity [(Int,Double)]) EitherString EitherString () [(Int,Double)] () [(Int,Double)] Double)
      ()
      ()
      ()
      (\f -> do
        it "empty" $ f mempty `shouldSatisfy` isString

        it "success" $ f [(1,2),(3,4)] `shouldBe` EValue (3,[(3,4)])

        it "expect fail" $ f [(2,3)] `shouldSatisfy` isString
      )
      \b -> do
        it "empty" $ b empty `shouldSatisfy` isString

        it "success" $ b [(1,3)] `shouldBe` EValue (4,[(1,3)])

  describe "lensBiparse" do
    let lb :: Constructor ([Int],()) EitherString EitherString () ()
        lb = lensBiparse _1
          (take 1 :: Unit () (Identity [Int]) EitherString EitherString () [Int] ())

    describe "forward" do
      let f = runForwardC lb
      it "success" $ f ([1],()) `shouldBe` EValue ()
      it "fail" $ f ([1],()) `shouldBe` EValue ()

    describe "backward" do
      let b = runBackwardC lb ()
      it "success" $ b ([],()) `shouldBe` EValue ((), ([1], ()))

  describe "expose" do
    let e = expose :: Constructor s Identity Identity u s

    it "forward" $ runForwardC e 'c' `shouldBe` Identity 'c'

    it "backward" $ runBackwardC e () 'c' `shouldBe` Identity ('c','c')

