module Biparse.GeneralSpec where

import Control.Monad.Except
import Data.ByteString.Char8 (ByteString)
import Data.Sequence (Seq)
import Data.Vector (Vector)

spec :: Spec
spec = do
  describe "take" do
    let bp :: Unit IdentityStateContext Text IO IO
        bp = take 'a'
        bp2 :: Unit IdentityStateContext Text IO IO
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

  describe "takeDi" do
    let bp :: Iso IdentityStateContext IO IO Text Int
        bp = takeDi 'x' 1

    describe "forward" do
      let f = runForward bp

      it "matches" $ f "xabc" >>= (`shouldBe` (1,"abc"))

      it "no matche" $ f "abc" `shouldThrow` isUserError

  describe "takeNot" do
    let bp :: Iso IdentityStateContext IO IO String Char
        bp = takeNot 'A'

    describe "forward" do
      let f = runForward bp

      it "takes non-matching element" do
        x <- f "bc"
        x `shouldBe` ('b', "c")

      it "does not take matching element" do
        f "Abc" `shouldThrow` isUserError

    describe "backward" do
      let b = runBackward bp

      it "prints non-matching" do
        x <- b 'c'
        x `shouldBe` ('c', "c")

      it "fails matching" do
        b 'A' `shouldThrow` isUserError

  fb
    "takeWhile"
    (takeWhile (/= 'x') :: Iso LineColumn IO IO (Position Text) Text)
    (\f -> do
      it "empty" do
        x <- f ""
        x `shouldBe` (mempty,"")

      it "take none" do
        x <- f "xab"
        x `shouldBe` (mempty, "xab")

      it "take 2" do
        x <- f "abx"
        x `shouldBe` ("ab", Position 1 3 "x")

      it "take all" do
        x <- f "abc"
        x `shouldBe` ("abc", Position 1 4 mempty)
    )
    \b -> do
      it "empty" do
        x <- b mempty
        x `shouldBe` (mempty,mempty)

      it "no x" do
        x <- b "abc"
        x `shouldBe` ("abc", "abc")

      it "has x" do
        x <- b "axc"
        x `shouldBe` ("axc", "axc")

  fb
    "breakWhen"
    (breakWhen $ stripPrefix "ab" :: Iso LineColumn IO IO (Position (Seq Char)) (Seq Char))
    (\f -> do
      it "empty" do
        x <- f ""
        x `shouldBe` (mempty, "")

      it "break first" do
        x <- f "abcd"
        x `shouldBe` (mempty, Position 1 3 "cd")

      it "break last" do
        x <- f "cdab"
        x `shouldBe` ("cd", Position 1 5 mempty)

      it "break middle" do
        x <- f "cdabef"
        x `shouldBe` ("cd", Position 1 5 "ef")

      it "no break" do
        x <- f "cdefg"
        x `shouldBe` ("cdefg", Position 1 6 mempty)
    )
    \b -> do
      it "empty" do
        x <- b mempty
        x `shouldBe` (mempty,"ab")

      it "append break" do
        x <- b "cd"
        x `shouldBe` ("cd", "cdab")

      it "only break" do
        x <- b "ab"
        x `shouldBe` ("ab", "abab")

      it "contains break" do
        x <- b "cdab"
        x `shouldBe` ("cdab", "cdabab")

  describe "optionMaybe" do
    let bp :: Biparser IdentityStateContext (Vector Int) IO IO Bool (Maybe String, Maybe String)
        bp = (,) <$> optionMaybe (takeUni 1 `upon` mapBool $> "one")
           <*> optionMaybe (takeUni 2 `upon` mapBool $> "two")
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

  describe "not" do
    let bp :: Biparser IdentityStateContext String IO IO Char Bool
        bp = not $ (== 'x') <$> one

    describe "forward" do
      let f = runForward bp

      it "true" $ f "ab" >>= (`shouldBe` (True,"b"))

      it "false" $ f "xb" >>= (`shouldBe` (False,"b"))

    describe "backward" do
      let b = runBackward bp

      it "true" $ b 'x' >>= (`shouldBe` (False,"x"))

      it "false" $ b 'a' >>= (`shouldBe` (True,"a"))

