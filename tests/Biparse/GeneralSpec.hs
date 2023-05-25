module Biparse.GeneralSpec where

import Biparse.BiparserT
import Biparse.General
import Control.Applicative
import Control.Monad.Trans.Except
import Data.Bifunctor
import Data.Functor
import Data.Functor.Identity
import Data.ByteString.Char8 (ByteString)
import Data.ByteString.Internal (w2c, c2w)
import Data.Char
import Data.Sequence (Seq)
import Data.Text (Text)
import Data.Word (Word8)
import Data.Vector (Vector)
import Prelude hiding (take, takeWhile)
import System.IO.Error (isUserError)
import Test.Hspec

spec :: Spec
spec = do
  describe "take" do
    let bp = take 'a' :: Unit Text IO IO
        bp2 :: Unit Text IO IO
        bp2 = take 'a' *> take 'b'

    describe "forward" do
      it "take matching" do
        x <- runForward bp "abc"
        x `shouldBe` ((), "bc")

      it "take two matching" do
        x <- runForward bp2 "abc"
        x `shouldBe` ((), "c")

      describe "fail" do
        it "non-matching" do
          runForward bp "bc" `shouldThrow` isUserError
      
        it "empty" do
          runForward bp mempty `shouldThrow` isUserError

    describe "backward" do
      it "print one" do
        x <- runBackward bp ()
        x `shouldBe` ((), "a")

      it "print two" do
        x <- runBackward bp2 ()
        x `shouldBe` ((), "ab")

  describe "takeWhile" do
    let bp :: Iso IO IO ByteString [Word8]
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

  describe "optionMaybe" do
    let bp :: BiparserT (Vector Int) IO IO Bool (Maybe String, Maybe String)
        bp = (,) <$> optionMaybe (take'' 1 `upon` mapBool $> "one")
           <*> optionMaybe (take'' 2 `upon` mapBool $> "two")
        mapBool :: Bool -> Int
        mapBool = \case True -> 1; False -> 2

    describe "forward" do
      it "matches both" do
        x <- runForward bp [1, 2]
        x `shouldBe` ((Just "one", Just "two"), mempty)

      it "matches first" do
        x <- runForward bp [1, 3]
        x `shouldBe` ((Just "one", Nothing), [3])

      it "matches second" do
        x <- runForward bp [2, 3]
        x `shouldBe` ((Nothing, Just "two"), [3])

      it "matches none" do
        x <- runForward bp mempty
        x `shouldBe` ((Nothing, Nothing), mempty)

    describe "backward" do
      it "prints first" do
        x <- runBackward bp True
        x `shouldBe` ((Just "one", Nothing), [1])

      it "prints second" do
        x <- runBackward bp False
        x `shouldBe` ((Nothing, Just "two"), [2])

