module Biparse.Biparser.StateWriterSpec where

import Biparse.Biparser.StateWriter 
--import Biparse.List (splitOn, all)
import Biparse.Text.Numeric (naturalBaseTen)
--import Control.Monad.Trans.Except (Except)

spec :: Spec
spec = do
  pure ()
  --fb
  --  "translate mantains shared state Line and Column"
  --  ( translate
  --    _
  --    (splitOn $ stripPrefix "\r\n")
  --  $ all 
  --    ( comap runIdentity $ fmap Identity
  --    $ takeUni "abc" <|> takeUni "def"
  --    :: Iso IdentityState (Except String) IO [String] (Identity String))
  --  :: Iso LineColumn (FM String) IO (Position String) [Identity String])
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

  fb "zoom"
    (zoom @LinesOnly @LinesOnly @(Position [Text]) @(Position Text) @(FM [Text]) @(FM Text)
      (one :: Iso LinesOnly (FM [Text]) IO (Position [Text]) Text)
      (naturalBaseTen :: Iso LinesOnly (FM Text) IO (Position Text) Word)
    :: Iso LinesOnly (FM [Text]) IO (Position [Text]) Word)
    (\f -> do
      it "empty" do
        f [] `shouldSatisfy` errorPosition 1 1

      describe "zooms and parses Word" do
        it "no tail" do
          f ["123"] `shouldBe` Right (123, Position 2 1 [])

        it "with tail" do
          f ["123","abc"] `shouldBe` Right (123, Position 2 1 ["abc"])
    )
    \b -> do
      it "prints Word" $ b 456 >>= (`shouldBe` (456, ["456"]))

  describe "runForward" do
    let bp :: Unit LineColumn (Position String) (FM String) IO
        bp = take 'a' *> take 'b'
        f :: Position String -> Either ErrorPosition ((), Position String)
        f = runForward bp

    it "position incremented correctly" $
      f "abc" `shouldBe` Right ((), Position 1 3 "c")

    it "integrates the state correctly with the error" $
      f "az" `shouldSatisfy` errorPosition 1 2

