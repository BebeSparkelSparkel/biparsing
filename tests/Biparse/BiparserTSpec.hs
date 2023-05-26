module Biparse.BiparserTSpec where

import Test.Hspec
import Biparse.BiparserT
import Biparse.General
import Data.Sequence (Seq)
import Control.Applicative
import Prelude hiding (take, takeWhile)
import System.IO.Error (isUserError)

spec :: Spec
spec = do
  describe "one" do
    let bp = one :: Iso IdentityStateContext IO IO String Char
        bp2 :: Iso IdentityStateContext IO IO String (Char,Char)
        bp2 = (,) <$> bp `upon` fst <*> bp `upon` snd

    describe "forward" do
      it "one use" do
        x <- runForward bp "abc"
        x `shouldBe` ('a',"bc")

      it "used twice" do
        x <- runForward bp2 "abc"
        x `shouldBe` (('a','b'),"c")
        
      it "none to take" do
        runForward bp mempty `shouldThrow` isUserError

    describe "backward" do
      it "typical use" do
        x <- runBackward bp 'a'
        x `shouldBe` ('a',"a")

      it "used twice" do
        x <- runBackward bp2 ('a','b')
        x `shouldBe` (('a','b'),"ab")

  describe "try" do
    let bp :: BiparserT IdentityStateContext (Seq Char) IO IO Char Char
        bp = try $ one <* take 'b'

    describe "forward" do
      it "success" do
        x <- runForward bp "abc"
        x `shouldBe` ('a',"c")
      
      it "does not consume state in failed attempt" do
        x <- runForward (bp <|> take'' 'c') "cde"
        x `shouldBe` ('c', "de")

      it "fails if no alternate" do
        runForward bp mempty `shouldThrow` isUserError

    describe "backward" do
      it "prints correctly" do
        x <- runBackward bp 'a'
        x `shouldBe` ('a',"ab")

      it "prints second if first fails (more of a test for the BiparserT Alternative instance and should proabaly moved there)" do
        x <- runBackward (bp {backward = const empty} <|> bp) 'z'
        x `shouldBe` ('z',"zb")
      

