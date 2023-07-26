module Biparse.Text.Context.LineColumnSpec where

import Biparse.Text.Context.LineColumn

spec :: Spec
spec = do
  describe "UpdateStateWithElement" do
    describe "simple" do
      let bp :: IsoLocal String Char
          bp = one

      describe "forward" do
        it "increments column" do
          runForward @() bp "ab" `shouldBe` Right ('a', Position 1 2 "b")

        it "increments row" do
          runForward @() bp "\nb" `shouldBe` Right ('\n', Position 2 1 "b")

        it "resets colum on newline" do
          runForward @() (bp >> bp) "a\nb" `shouldBe` Right ('\n', Position 2 1 "b")

      describe "backward" do
        it "prints a character" do
          x <- runBackward bp 'a'
          x `shouldBe` ('a', "a")

        it "prints newline" do
          x <- runBackward bp '\n'
          x `shouldBe` ('\n', "\n")

  describe "UpdateStateWithSubState" do
    let bp :: IsoLocal Text Text
        bp = takeWhile (/= ':')

    describe "forward" do
      let f :: Position Text -> Either ErrorPosition (Text, Position Text)
          f = runForward @() bp

      it "no splitter" do
        f "abc" `shouldBe` Right ("abc", Position 1 4 mempty)

      it "no newline consumed" do
        f "ab:de" `shouldBe` Right ("ab", Position 1 3 ":de")

      it "newline column 1" do
        f "a\n:b" `shouldBe` Right ("a\n", Position 2 1 ":b")

      it "newline column 3" do
        f "ab\n\n\ncd:e" `shouldBe` Right ("ab\n\n\ncd", Position 4 3 ":e")

      prop "never zero" \t -> do
        let Right (_, Position l c _) = f $ startLineColumn t
        l `shouldSatisfy` (> 0)
        c `shouldSatisfy` (> 0)

    describe "backward" do
      let b = runBackward bp

      prop "writes all" \t -> b t >>= (`shouldBe` (t, t))

  describe "add position to error" do
    let bp :: Const LineColumn (Position Text) (Either (ErrorState String (Position Text))) IO ()
        bp = take 'a' *> take 'b'
        f :: Position Text -> Either ErrorPosition ((), Position Text)
        f = runForward @() bp

    it "empty" do
      f "" `shouldSatisfy` errorPosition 1 1

type IsoLocal text a = Iso LineColumn (FM text) IO (Position text) a

