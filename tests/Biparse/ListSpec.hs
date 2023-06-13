module Biparse.ListSpec where

import Biparse.BiparserT
import Biparse.General
import Biparse.List
import Control.Applicative hiding (many,some)
import Data.Bifunctor
import Data.ByteString (ByteString)
import Data.ByteString.Internal (w2c, c2w)
import Data.Char
import Data.Functor
import Data.Functor.Identity
import Data.List.NonEmpty (NonEmpty)
import Data.Maybe
import Data.MonoTraversable (headMay)
import Data.String
import Data.Text (Text)
import Data.Text qualified as T
import Data.Word (Word8)
import Prelude hiding (take, not)
import Data.Bool qualified as Data.Bool
import System.IO.Error (isUserError)
import System.Timeout (timeout)
import Test.Hspec
import Test.Hspec.QuickCheck
import Control.Monad.Trans.State.Lazy
import Data.Sequences (tailMay)
import Test.QuickCheck

spec :: Spec
spec = do
  describe "takeElementsWhile" do
    let bp :: Iso IdentityStateContext IO IO ByteString [Word8]
        bp = takeElementsWhile (isDigit . w2c)
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
    describe "with takeUni" do
      let bp :: Iso IdentityStateContext IO IO Text [Char]
          bp = many $ takeUni 'a'

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

    describe "with takeTri" do
      let bp :: BiparserT IdentityStateContext [String] Maybe Maybe [Bool] [Int]
          bp = many
            $   try (takeTri "TRUE" True 1)
            <|>      takeTri "FALSE" False 0

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
        bp = some (takeUni 1)

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

  describe "whileM" do
    let bp :: Iso IdentityStateContext IO IO Text [Char]
        bp = whileM (peek (memptyBack one >>= \x -> pure $ x /= 'x')) one

    describe "forward" do
      let f = runForward bp

      it "empty" do
        x <- f mempty
        x `shouldBe` (mempty,mempty)

      it "takes all" do
        x <- f "abc"
        x `shouldBe` ("abc",mempty)

      it "takes none" do
        x <- f "x"
        x `shouldBe` (mempty,"x")

      it "takes till x" do
        x <- f "abxc"
        x `shouldBe` ("ab","xc")

    describe "backward" do
      let b = runBackward bp

      it "empty" do
        x <- b mempty
        x `shouldBe` (mempty,mempty)

      it "prints all" do
        x <- b "abc"
        x `shouldBe` ("abc","abc")

      it "prints till x" do
        x <- b "abxc"
        x `shouldBe` ("ab","ab")

  describe "whileId" do
    describe "THIS IS WACK" do
      let o = one :: Iso IdentityStateContext Maybe Maybe Text Char
          m2i x = Identity . fromMaybe (False,x)
          bp :: Iso IdentityStateContext Identity Identity Text [Maybe Char]
          bp = whileId
            ( memptyBack $ mapMs' m2i m2i $ comap (fromMaybe '0') $ peek do
              x <- o
              pure $ x /= 'x'
            )
            (fmap pure $ mapMs (pure . fromJust) (pure . fromJust) $ (o `uponMay` '0') id)
  
      describe "forward" do
        let f = runForward bp
  
        it "empty" do
          f mempty `shouldBe` Identity (mempty,mempty)
  
        it "takes all" do
          f "abc" `shouldBe` Identity ("abc",mempty)
  
        it "takes none" do
          f "x" `shouldBe` Identity (mempty,"x")
  
        it "takes till x" do
          f "abxc" `shouldBe` Identity ("ab","xc")
  
      describe "backward" do
        let b = runBackward bp
  
        it "empty" do
          b mempty `shouldBe` Identity (mempty,mempty)
  
        it "prints all" do
          b "abc" `shouldBe` Identity ("abc","abc")
  
        it "prints till x" do
          b "abxc" `shouldBe` Identity ("ab","ab")

  describe "untilId" do
    let bp :: Iso IdentityStateContext Identity Identity String [String]
        bp = untilId isNull bp'
        bp' :: Iso IdentityStateContext Identity Identity String String
        bp' = split do
          x <- get
          maybe undefined (\(y,z) -> y <$ put z) $
            liftA2 (,) (pure <$> headMay x) (tailMay x)

    describe "forward" do
      let f = runForward bp

      it "mempty" $ f mempty `shouldBe` Identity (mempty,mempty)

      it "takes all" $ f "ab" `shouldBe` Identity (["a","b"],mempty)

    describe "backward" do
      let b = runBackward bp

      it "mempty" $ b mempty `shouldBe` Identity (mempty,mempty)

      it "takes all" $ b ["a","b"] `shouldBe` Identity (["a","b"],"ab")

instance {-# OVERLAPS #-} IsString [Maybe Char] where fromString = fmap pure
