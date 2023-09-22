module Biparse.Biparser.StateReaderWriterSpec where

import Biparse.Biparser.StateReaderWriter
import Biparse.List (all)
import Biparse.Text.LineBreak (lines, LineBreakType(Unix))
import Biparse.Text.Context.LineColumn (ListToElement, ElementToList)
import Biparse.Text.Numeric (naturalBaseTen')

spec :: Spec
spec = do
  pure ()
  --fb
  --  "translate mantains shared state Line and Column"
  --  ( translate
  --    _
  --    (splitWith $ stripPrefix "\r\n")
  --  $ all 
  --    ( comap runIdentity $ fmap Identity
  --    $ takeUni "abc" <|> takeUni "def"
  --    :: Iso IdentityState (Except String) IO [String] (Identity String))
  --  :: Iso UnixLC (FM String) IO (Position String) [Identity String])
  --  (\f -> do
  --    it "empty" do
  --      f "" `shouldBe` Right (mempty, Position 1 1 mempty)

  --    it "no endline" do
  --      f "abc" `shouldBe` Right (["abc"], Position 1 4 mempty)

  --    it "two" do
  --      f "abc\r\ndef" `shouldBe` Right (["abc","def"], Position 2 4 mempty)

  --    describe "fail" do
  --      it "match none" do
  --        f "ghi\r\n" `shouldSatisfy` errorPosition 1 1

  --      it "match one and fail" do
  --        f "abc\r\nghi\r\n" `shouldSatisfy` errorPosition 2 1
  --  )
  --  \b -> do
  --    it "empty" do
  --      x <- b mempty
  --      x `shouldBe` (mempty, mempty)

  --    it "one" do
  --      x <- b ["abc"]
  --      x `shouldBe` (["abc"], "abc")

  --    it "two" do
  --      let ls = ["def", "abc"]
  --      x <- b ls
  --      x `shouldBe` (ls, "def\r\nabc")

  --    it "fail" do
  --      b ["ghi"] `shouldThrow` isUserError

  describe "zoom" do
    fb @() "[Text] -> Text, one line"
      (zoom @ElementToList @LinesOnly @LinesOnly @(Position [Text]) @(Position Text) @(FM [Text]) @(FM Text)
        --one
        (one :: Iso LinesOnly (FM [Text]) IO () (Position [Text]) Text)
        (naturalBaseTen' @Word)
      )
      ()
      (\f -> do
        it "empty" $ f [] `shouldSatisfy` errorPosition 1 1

        describe "zooms and parses Word" do
          it "no tail" $
            f ["123"] `shouldBe` Right (123, Position 2 1 [])

          it "with tail" $
            f ["123","abc"] `shouldBe` Right (123, Position 2 1 ["abc"])
      )
      \b -> do
        it "prints Word" $
          b 456 >>= (`shouldBe` (456, ["456"]))

    fb @() "Text -> [Text], all lines"
      (zoom @ListToElement @LinesOnly @UnixLC @(Position Text) @(Position [Text]) @(FM Text) @(FM [Text])
        (lines @'Unix)
        ( all
        $ zoom @ElementToList @UnixLC @_ @_ @_ @_ @(FM Text)
          one
          (naturalBaseTen' @Int)
        )
      )
      ()
      (\f -> do
        it "empty" $ f "" `shouldBe` Right ([],"")

        it "single line no newline character" $
          f "123" `shouldBe` Right ([123], Position 1 4 "")

        it "single line with newline" $
          f "123\n" `shouldSatisfy` errorPosition 2 1

        it "two lines" $
          f "123\n456" `shouldBe` Right ([123,456], Position 2 4 "")

        it "two lines" $
          f "123\n456\n" `shouldSatisfy` errorPosition 3 1

        it "fail" $
          f "123\nabc" `shouldSatisfy` errorPosition 2 1
      )
      \b -> do
        it "prints all" $
          b [123,456] >>= (`shouldBe` ([123,456], "123\n456"))

  describe "runForward" do
    let bp :: Unit UnixLC (Position String) (FM String) IO ()
        bp = take 'a' *> take 'b'
        f :: Position String -> Either ErrorPosition ((), Position String)
        f = runForward @() bp

    it "position incremented correctly" $
      f "abc" `shouldBe` Right ((), Position 1 3 "c")

    it "integrates the state correctly with the error" $
      f "az" `shouldSatisfy` errorPosition 1 2

  it "runBackward" do
    let bp :: Iso IdentityState IO EitherString Char ByteString ByteString
        bp = do
          c <- ask' undefined
          cons (c2w c) <$> rest
        b = runBackward bp 'A'
    b "cd" `shouldBe` EValue ("Acd", "cd")

