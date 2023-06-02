module Biparse.Text.PositionContextSpec where

import Test.QuickCheck.Instances.Text ()
import Test.Hspec
import Test.Hspec.QuickCheck
import Biparse.Text.PositionContext
import Biparse.General
import Biparse.BiparserT
import Data.Text (Text)
import Prelude hiding (takeWhile)

spec :: Spec
spec = do
  describe "UpdateStateWithElement" do
    describe "simple" do
      let bp :: Iso LineColumn IO IO (Position String) Char
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

  describe "UpdateStateWithSubState" do
    let bp :: Iso LineColumn IO IO (Position Text) Text
        bp = takeWhile (/= ':')

    describe "forward" do
      let f = runForward bp

      it "no splitter" do
        x <- f "abc"
        x `shouldBe` ("abc", Position 1 4 mempty)

      it "no newline consumed" do
        x <- f "ab:de"
        x `shouldBe` ("ab", Position 1 3 ":de")

      it "newline column 1" do
        x <- f "a\n:b"
        x `shouldBe` ("a\n", Position 2 1 ":b")

      it "newline column 3" do
        x <- f "ab\n\n\ncd:e"
        x `shouldBe` ("ab\n\n\ncd", Position 4 3 ":e")

      prop "never zero" \t -> do
        (_, Position l c _) <- f $ startPosition t
        l `shouldSatisfy` (> 0)
        c `shouldSatisfy` (> 0)

    describe "backward" do
      let b = runBackward bp

      prop "writes all" \t -> do
        x <- b t
        x `shouldBe` (t, t)

