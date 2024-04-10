{-# LANGUAGE PolyKinds #-}
module Biparse.Biparser.StateReaderWriterSpec where

import Biparse.Biparser.StateReaderWriter (zoom)
import Biparse.Text.LineBreak (lines, LineBreakType(Unix))
import Biparse.Text.Numeric (naturalBaseTen')

spec :: Spec
spec = do
  describe "zoom" do
    fb @() "[Text] -> Text, one line"
      (zoom @ElementToList @LinesOnly @Either
        (one :: Iso LinesOnly (FM [Text]) IO () [Text] () (Position () [Text]) Text)
        (naturalBaseTen' @Word)
      :: Iso LinesOnly (FM [Text]) IO () [Text] () (Position () [Text]) Word)
      ()
      ()
      (\f -> do
        it "empty" $ f [] `shouldSatisfy` errorPosition 1 1

        describe "zooms and parses Word" do
          it "no tail" $
            f ["123"] `shouldBe` Right (123, Position () 2 1 [])

          it "with tail" $
            f ["123","abc"] `shouldBe` Right (123, Position () 2 1 ["abc"])
      )
      \b -> do
        it "prints Word" $
          b 456 >>= (`shouldBe` (456, ["456"]))

    fb @() "Text -> [Text], all lines"
      (zoom @ListToElement @LinesOnly @Either
        (lines @'Unix)
        ( all
        $ zoom @ElementToList @UnixLC @Either
          (one @[Text])
          (naturalBaseTen' @Int)
        )
      :: Iso UnixLC (FM Text) IO () Text () (Position () Text) [Int])
      ()
      ()
      (\f -> do
        it "empty" $ f "" `shouldBe` Right ([],"")

        it "single line no newline character" $
          f "123" `shouldBe` Right ([123], Position () 1 4 "")

        it "single line with newline" $
          f "123\n" `shouldSatisfy` errorPosition 2 1

        it "two lines" $
          f "123\n456" `shouldBe` Right ([123,456], Position () 2 4 "")

        it "two lines" $
          f "123\n456\n" `shouldSatisfy` errorPosition 3 1

        it "fail" $
          f "123\nabc" `shouldSatisfy` errorPosition 2 1
      )
      \b -> do
        it "prints all" $
          b [123,456] >>= (`shouldBe` ([123,456], "123\n456"))

  describe "runForward" do
    let bp :: Unit UnixLC (Position () String) (FM String) IO () String ()
        bp = take 'a' *> take 'b'
        f :: Position () String -> Either (ErrorPosition ()) ((), Position () String)
        f = runForward @() bp

    it "position incremented correctly" $
      f "abc" `shouldBe` Right ((), Position () 1 3 "c")

    it "integrates the state correctly with the error" $
      f "az" `shouldSatisfy` errorPosition 1 2

  it "runBackward" do
    let bp :: Iso () IO EitherString Char ByteString () (Identity ByteString) ByteString
        bp = do
          c <- askBw undefined
          cons (c2w c) <$> rest
        b = runBackward bp 'A' ()
    b "cd" `shouldBe` EValue ("Acd", "cd")

