module Biparse.Biparser.StateWriterSpec where

import Biparse.Biparser.StateWriter 
import Biparse.List (splitOn, all)

spec :: Spec
spec = do

  fb
    "translate mantains shared state Line and Column"
    (( translate @IdentityStateContext
     (splitOn $ stripPrefix "\r\n")
    $ all
      $   takeUni "abc"
      <|> takeUni "def"
    ) :: Iso LineColumn IO IO (Position String) [String]
    )
    (\f -> do
      it "empty" do
        x <- f ""
        x `shouldBe` (mempty, Position 1 1 mempty)

      it "no endline" do
        x <- f "abc"
        x `shouldBe` (["abc"], Position 1 4 mempty)

      --it "one" do
      --  x <- f "abc\r\n"
      --  x `shouldBe` (["abc"], Position 2 1 mempty)

      it "two" do
        x <- f "abc\r\ndef"
        x `shouldBe` (["abc","def"], Position 2 4 mempty)
      --it "two" do
      --  x <- f "abc\r\ndef\r\n"
      --  x `shouldBe` (["abc","def"], Position 3 1 mempty)

      it "match none and fail" do
        f "ghi\r\n" `shouldThrow` isUserError
      --it "match none and consume all" do
      --  x <- f "ghi\r\n"
      --  x `shouldBe` (mempty, Position 2 1 mempty)
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
      --it "fail" do
      --  x <- b ["ghi"] -- `shouldThrow` isUserError
      --  x `shouldBe` (mempty,mempty)

