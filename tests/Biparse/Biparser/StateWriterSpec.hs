module Biparse.Biparser.StateWriterSpec where

import Biparse.Biparser.StateWriter 
import Biparse.List (splitOn, all)
import Biparse.Text.Numeric (naturalBaseTen)

spec :: Spec
spec = do
  fb
    "translate mantains shared state Line and Column"
    ( translate
      (splitOn $ stripPrefix "\r\n")
    $ all 
      ( comap runIdentity $ fmap Identity
      $ takeUni "abc" <|> takeUni "def"
      :: Iso IdentityStateContext IO IO [String] (Identity String)
      )
    :: Iso LineColumn IO IO (Position String) [Identity String]
    )
    (\f -> do
      it "empty" do
        x <- f ""
        x `shouldBe` (mempty, Position 1 1 mempty)

      it "no endline" do
        x <- f "abc"
        x `shouldBe` (["abc"], Position 1 4 mempty)

      it "two" do
        x <- f "abc\r\ndef"
        x `shouldBe` (["abc","def"], Position 2 4 mempty)

      it "match none and fail" do
        f "ghi\r\n" `shouldThrow` isUserError
    )
    \b -> do
      it "empty" do
        x <- b mempty
        x `shouldBe` (mempty, mempty)

      it "one" do
        x <- b ["abc"]
        x `shouldBe` (["abc"], "abc")

      it "two" do
        let ls = ["def", "abc"]
        x <- b ls
        x `shouldBe` (ls, "def\r\nabc")

      it "fail" do
        b ["ghi"] `shouldThrow` isUserError

  fb "zoom"
    (zoom @LinesOnly @LinesOnly @(Position [Text]) @(Position Text)
      (one :: Iso LinesOnly IO IO (Position [Text]) Text)
      (naturalBaseTen :: Iso LinesOnly IO IO (Position Text) Word)
    :: Iso LinesOnly IO IO (Position [Text]) Word)
    (\f -> do
      it "empty" do
        f [] `shouldThrow` isUserError

      describe "zooms and parses Word" do
        it "no tail" do
          x <- f ["123"]
          x `shouldBe` (123, Position 2 1 [])

        it "with tail" do
          x <- f ["123","abc"]
          x `shouldBe` (123, Position 2 1 ["abc"])
    )
    \b -> do
      it "prints Word" do
        x <- b 456
        x `shouldBe` (456, ["456"])

