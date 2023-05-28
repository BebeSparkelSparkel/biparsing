module Biparse.ListSpec where

import System.IO.Error (isUserError)
import System.Timeout
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Biparse.BiparserT
import Biparse.General
import Data.ByteString.Internal (w2c, c2w)
import Data.Bifunctor
import Data.Char
import Control.Applicative hiding (many,some)
import Data.List.NonEmpty (NonEmpty)
import Biparse.List
import Data.ByteString (ByteString)
import Prelude hiding (take, takeWhile)
import Data.Word (Word8)
import Data.Text (Text)
import Data.Text qualified as T

spec :: Spec
spec = do
  describe "takeWhile" do
    let bp :: Iso IdentityStateContext IO IO ByteString [Word8]
        bp = takeWhile (isDigit . w2c)
        pretty = first (fmap w2c)

    describe "forward" do
      it "matches some digits" do
        x <- runForward bp "123 abc"
        pretty x `shouldBe` ("123", " abc")

      it "matches all characters" do
        x <- runForward bp "123"
        pretty x `shouldBe` ("123", mempty)

      it "matches no digits" do
        x <- runForward bp "abc"
        x `shouldBe` (mempty, "abc")

      it "empty" do
        x <- runForward bp mempty
        x `shouldBe` (mempty, mempty)

    describe "backward" do
      it "prints some" do
        x <- runBackward bp (c2w <$> "abc")
        pretty x `shouldBe` ("abc", "abc")

      it "prints none" do
        x <- runBackward bp mempty
        pretty x `shouldBe` (mempty, mempty)

  describe "many" do
    describe "with take''" do
      let bp :: Iso IdentityStateContext IO IO Text [Char]
          bp = many $ take'' 'a'

      describe "forward" do
        let f = runForward bp
        
        it "takes none" do
          x <- f mempty
          x `shouldBe` (mempty, mempty)

        it "takes 2" do
          x <- f "aabc"
          x `shouldBe` (['a','a'], "bc")

      describe "backward" do
        let b = runBackward bp

        it "prints all" do
          x <- b ['a','a']
          x `shouldBe` ("aa", "aa")

        it "prints none" do
          x <- b mempty
          x `shouldBe` (mempty, mempty)

    describe "with take'''" do
      let bp :: BiparserT IdentityStateContext [String] Maybe Maybe [Bool] [Int]
          bp = many
            $   try (take''' "TRUE" True 1)
            <|>      take''' "FALSE" False 0 

      describe "forward" do
        it "takes two" do
          runForward bp ["TRUE","FALSE","UNDEFINED"]
            `shouldBe` Just ([1,0],["UNDEFINED"])

        it "takes none" do
          runForward bp ["UNDEFINED"]
            `shouldBe` Just (mempty, ["UNDEFINED"])
          runForward bp mempty
            `shouldBe` Just (mempty, mempty)

      describe "backward" do
        it "prints all" do
          runBackward bp [False, True]
            `shouldBe` Just ([0,1], ["FALSE", "TRUE"])

        it "prints none" do
          runBackward bp mempty
            `shouldBe` Just (mempty, mempty)

  describe "some" do
    let bp :: Iso IdentityStateContext IO IO [Int] (NonEmpty Int)
        bp = some (take'' 1)
    
    describe "forward" do
      let f = runForward bp

      it "fails on none" do
        f mempty `shouldThrow` isUserError
        f [2] `shouldThrow` isUserError

      it "takes some" do
        f [1] >>= (`shouldBe` ([1], mempty))
        f [1,2] >>= (`shouldBe` ([1], [2]))
        f [1,1,2] >>= (`shouldBe` ([1,1], [2]))

    describe "backward" do
      let b = runBackward bp

      it "prints all" do
        x <- b [1,1,1]
        x `shouldBe` ([1,1,1], [1,1,1])

  describe "splitElem" do
    let bp :: Iso IdentityStateContext IO IO Text [Text]
        bp = splitElem ':'
        f = runForward bp
        b = runBackward bp
        t name f' b' = describe name do
          it "forward" do
            x <- timeout 1000 $ f f'
            x `shouldBe` Just (b', mempty)
          it "backward" do
            y <- timeout 1000 $ b b'
            y `shouldBe` Just (b', f')

    t "empty" mempty mempty
    t "one element no splits" "a" ["a"]
    t "only split element" ":" [mempty,mempty]
    t "triple split" ":a:bc" [mempty,"a","bc"]
    t "empty last" "ab:" ["ab",mempty]
    t "empty first" ":ab" [mempty,"ab"]

    let ef = evalForward bp
    prop "forward should never return [\"\"]" $ forAll (T.pack <$> listOf (elements "ab:")) \string -> do
      f' <- ef string
      f' `shouldNotBe` [mempty]

