module Biparse.Text.NumericSpec where

import Biparse.Text.Numeric

spec :: Spec
spec = do
  fb "naturalBaseTen"
    (naturalBaseTen :: Iso LineColumn IO IO (Position Text) Word)
    (\f -> do
      it "success" do
        f "0"    >>= (`shouldBe` (0,   Position 1 2 mempty))
        f "1"    >>= (`shouldBe` (1,   Position 1 2 mempty))
        f "123"  >>= (`shouldBe` (123, Position 1 4 mempty))
        f "123x" >>= (`shouldBe` (123, Position 1 4 "x"))

      it "fail" do
        f ""     `shouldThrow` isUserError
        f " 123" `shouldThrow` isUserError
    )
    \b -> do
      it "success" do
        b 1   >>= (`shouldBe` (1,   "1"))
        b 123 >>= (`shouldBe` (123, "123"))

  fb "intBaseTen"
    (intBaseTen :: Iso LineColumn IO IO (Position Text) Int)
    (\f -> do
      it "success" do
        f "1"     >>= (`shouldBe` (1,   Position 1 2 mempty))
        f "123"   >>= (`shouldBe` (123, Position 1 4 mempty))
        f "123x"  >>= (`shouldBe` (123, Position 1 4 "x"))
        f "-1"    >>= (`shouldBe` (-1,   Position 1 3 mempty))
        f "-123"  >>= (`shouldBe` (-123, Position 1 5 mempty))
        f "-123x" >>= (`shouldBe` (-123, Position 1 5 "x"))

      it "fail" do
        f "" `shouldThrow` isUserError
        f " 123" `shouldThrow` isUserError
        f " -123" `shouldThrow` isUserError
    )
    \b -> do
      it "success" do
        b 1      >>= (`shouldBe` (1,    "1"))
        b 123    >>= (`shouldBe` (123,  "123"))
        b (-1)   >>= (`shouldBe` (-1,   "-1"))
        b (-123) >>= (`shouldBe` (-123, "-123"))


  fb "realBaseTen"
    (realBaseTen :: Iso LineColumn IO IO (Position String) Double)
    (\f -> do
      it "whole" do
        f "1"    >>= (`shouldBe` (1,   Position 1 2 mempty))
        f "123"  >>= (`shouldBe` (123, Position 1 4 mempty))
        f "123x" >>= (`shouldBe` (123, Position 1 4 "x"))

      it "decimal" do
        f "1.0"     >>= (`shouldBe` (1.0,     Position 1 4 mempty))
        f "123.123" >>= (`shouldBe` (123.123, Position 1 8 mempty))

      it "negative" do
        f "-1.0"     >>= (`shouldBe` (-1.0,     Position 1 5 mempty))
        f "-123.123" >>= (`shouldBe` (-123.123, Position 1 9 mempty))

      it "fail" do
        f ""     `shouldThrow` isUserError
        f " 123" `shouldThrow` isUserError
        f "1."   `shouldThrow` isUserError
    )
    \b -> do
      it "whole" do
        b 1   >>= (`shouldBe` (1,   "1.0"))
        b 123 >>= (`shouldBe` (123, "123.0"))

      it "decimal" do
        b (1.23)   >>= (`shouldBe` (1.23,   "1.23"))
        b (123.45) >>= (`shouldBe` (123.45, "123.45"))

      it "negative" do
        b (-1)      >>= (`shouldBe` (-1,      "-1.0"))
        b (-123)    >>= (`shouldBe` (-123,    "-123.0"))
        b (-1.23)   >>= (`shouldBe` (-1.23,   "-1.23"))
        b (-123.45) >>= (`shouldBe` (-123.45, "-123.45"))
