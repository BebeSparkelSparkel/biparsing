module Biparse.Text.NumericSpec where

import Biparse.Text.Numeric
import GHC.Num (Num)
import GHC.Real (Fractional)

spec :: Spec
spec = do
  fb @() "naturalBaseTen"
    (naturalBaseTen :: Iso UnixLC (FM Text) IO () () (Position Text) Word)
    ()
    ()
    (\f -> do
      naturalsForward f
      failIntegerForward f
    )
    \b -> do
      it "success" do
        b 1   >>= (`shouldBe` (1,   "1"))
        b 123 >>= (`shouldBe` (123, "123"))

  fb @() "intBaseTen"
    (intBaseTen :: Iso UnixLC (FM Text) IO () () (Position Text) Int)
    ()
    ()
    (\f -> do
      describe "success" do
        naturalsForward f
        negativeIntegerForward f

      failIntegerForward f
    )
    \b -> do
      integerBackward b

  fb @() "scientific"
    (scientific :: Iso ColumnsOnly (FM Text) IO () () (Position Text) Double)
    ()
    ()
    (\f -> do
      realForward f

      it "10s power mulitiplier" do
        f "1e1" `shouldBe` Right (10, Position 1 4 mempty)
        f "-1.0E-1" `shouldBe` Right (-0.1, Position 1 8 mempty)
        f "12.345E2" `shouldBe` Right (1234.5, Position 1 9 mempty)
    )
    \b -> do
      realBackward b

  fb @() "realBaseTen"
    (realBaseTen :: Iso UnixLC (FM String) IO () () (Position String) Double)
    ()
    ()
    realForward
    realBackward

naturalsForward :: (Show a1, Show a2, Show text, Eq a1, Eq a2, Eq text, Num a2, Monoid text, IsString t, IsString text) => (t -> Either a1 (a2, Position text)) -> SpecWith ()
naturalsForward f = it "naturals" do
  f "0"    `shouldBe` Right (0,   Position 1 2 mempty)
  f "1"    `shouldBe` Right (1,   Position 1 2 mempty)
  f "123"  `shouldBe` Right (123, Position 1 4 mempty)
  f "123x" `shouldBe` Right (123, Position 1 4 "x")

negativeIntegerForward :: (Show a1, Show a2, Show text, Eq a1, Eq a2, Eq text, Num a2, Monoid text, IsString t, IsString text) => (t -> Either a1 (a2, Position text)) -> SpecWith ()
negativeIntegerForward f = it "negative integer" do
  f "-1"    `shouldBe` Right (-1,   Position 1 3 mempty)
  f "-123"  `shouldBe` Right (-123, Position 1 5 mempty)
  f "-123x" `shouldBe` Right (-123, Position 1 5 "x")

failIntegerForward :: (Show b, IsString t) => (t -> Either ErrorPosition b) -> SpecWith ()
failIntegerForward f = it "fail integer" do
  f ""      `shouldSatisfy` errorPosition 1 1
  f " 123"  `shouldSatisfy` errorPosition 1 1
  f " -123" `shouldSatisfy` errorPosition 1 1

integerBackward :: (Show a, Show b, Eq a, Eq b, Num t, Num a, IsString b) => (t -> IO (a, b)) -> SpecWith ()
integerBackward b = it "integer" do
  b 0      >>= (`shouldBe` (0,    "0"))
  b 1      >>= (`shouldBe` (1,    "1"))
  b 123    >>= (`shouldBe` (123,  "123"))
  b (-1)   >>= (`shouldBe` (-1,   "-1"))
  b (-123) >>= (`shouldBe` (-123, "-123"))

realForward :: (Show a, Show text, Eq a, Eq text, Monoid text, IsString t, IsString text, Fractional a) => (t -> Either ErrorPosition (a, Position text)) -> Spec
realForward f = do
  naturalsForward f

  it "decimal" do
    f "1.0"     `shouldBe` Right (1.0,     Position 1 4 mempty)
    f "123.123" `shouldBe` Right (123.123, Position 1 8 mempty)

  it "negative" do
    f "-1.0"     `shouldBe` Right (-1.0,     Position 1 5 mempty)
    f "-123.123" `shouldBe` Right (-123.123, Position 1 9 mempty)

  describe "fail" do
    failIntegerForward f

    it "fail real" do
      f "1."   `shouldSatisfy` errorPosition 1 3

realBackward :: (Show a, Show b, Eq a, Eq b, IsString b, Fractional t, Fractional a) => (t -> IO (a, b)) -> Spec
realBackward b = do
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

