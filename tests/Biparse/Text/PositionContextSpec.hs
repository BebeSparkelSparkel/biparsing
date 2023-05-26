module Biparse.Text.PositionContextSpec where

import Test.Hspec
import Biparse.Text.PositionContext
import Biparse.General
import Biparse.BiparserT

spec :: Spec
spec = do
  describe "simple" do
    let bp :: BiparserT LineColumn (Position String) IO IO Char Char
        bp = one

    describe "forward" do
      it "increments column" do
        x <- runForward bp "ab"
        x `shouldBe` ('a', Position 1 2 "b")

      it "increments row" do
        x <- runForward bp "\nb"
        x `shouldBe` ('\n', Position 2 1 "b")

      it "resets colum on newline" do
        x <- runForward (bp >> bp) "a\nb"
        x `shouldBe` ('\n', Position 2 1 "b")

    describe "backward" do
      it "prints a character" do
        x <- runBackward bp 'a'
        x `shouldBe` ('a', "a")

      it "prints newline" do
        x <- runBackward bp '\n'
        x `shouldBe` ('\n', "\n")

