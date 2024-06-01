module Biparse.Text.NumericSpec where

import Biparse.Text.Numeric

spec :: Spec
spec = do
  fb "naturalBaseTen"
    (naturalBaseTen :: Iso UnixLC (FM Text) IO () Text () (Position () Text) Word)
    ()
    ()
    ()
    (\f -> do
      naturalsForward f
      failIntegerForward f
    )
    \b -> do
      it "success" do
        b 1   `shouldReturn` (1,   "1")
        b 123 `shouldReturn` (123, "123")

  fb "intBaseTen"
    (intBaseTen :: Iso UnixLC (FM Text) IO () Text () (Position () Text) Int)
    ()
    ()
    ()
    (\f -> do
      describe "success" do
        naturalsForward f
        negativeIntegerForward f

      it "error shows failed to parse string" $
        f "   abc" `shouldSatisfy` errorPosition' (Just "Could not parse \"   abc\" to a base 10 natural.") 1 1

      failIntegerForward f
    )
    \b -> do
      integerBackward b

  fb "eNotation"
    (eNotation :: Iso ColumnsOnly (FM Text) IO () Text () (Position () Text) Double)
    ()
    ()
    ()
    (\f -> do
      realForward f

      it "10s power mulitiplier" do
        f "1e1" `shouldBe` Right (10, Position () 1 4 mempty)
        f "-1.0E-1" `shouldBe` Right (-0.1, Position () 1 8 mempty)
        f "12.345E2" `shouldBe` Right (1234.5, Position () 1 9 mempty)
    )
    \b -> do
      realBackward b

  fb "realBaseTen"
    (realBaseTen :: Iso UnixLC (FM String) IO () String () (Position () String) Double)
    ()
    ()
    ()
    realForward
    realBackward

  fb "hex"
    (hex @'LowerCase 2 :: Iso UnixLC (FM String) IO () String () (Position () String) HexWord8)
    ()
    ()
    ()
    (\f -> do
      it "zero" $ f "00" `shouldBe` Right (0, Position () 1 3 mempty)

      describe "bit order" do
        it "right" $ f "0f" `shouldBe` Right (0xf,  Position () 1 3 mempty)
        it "left"  $ f "F0" `shouldBe` Right (0xF0, Position () 1 3 mempty)

      let hexChars = elements $ fst <$> (digitsHexList <> capitalHexList <> lowerHexList :: [(Char,Word8)])
      prop "success" $ forAll (sequence [hexChars, hexChars]) \cs ->
        f (startLineColumn cs) `shouldSatisfy` isRight
    )
    \b -> do
      describe "bit order" do
        it "right" $ b 0xf  `shouldReturn` (0xf,  "0f")
        it "left"  $ b 0xF0 `shouldReturn` (0xF0, "f0")

naturalsForward :: (Show a1, Show a2, Show text, Eq a1, Eq a2, Eq text, Num a2, Monoid text, IsString t, IsString text) => (t -> Either a1 (a2, Position () text)) -> SpecWith ()
naturalsForward f = it "naturals" do
  f "0"    `shouldBe` Right (0,   Position () 1 2 mempty)
  f "1"    `shouldBe` Right (1,   Position () 1 2 mempty)
  f "123"  `shouldBe` Right (123, Position () 1 4 mempty)
  f "123x" `shouldBe` Right (123, Position () 1 4 "x")

negativeIntegerForward :: (Show a1, Show a2, Show text, Eq a1, Eq a2, Eq text, Num a2, Monoid text, IsString t, IsString text) => (t -> Either a1 (a2, Position () text)) -> SpecWith ()
negativeIntegerForward f = it "negative integer" do
  f "-1"    `shouldBe` Right (-1,   Position () 1 3 mempty)
  f "-123"  `shouldBe` Right (-123, Position () 1 5 mempty)
  f "-123x" `shouldBe` Right (-123, Position () 1 5 "x")

failIntegerForward :: (Show b, IsString t, Show text) => (t -> EESP () text b) -> SpecWith ()
failIntegerForward f = it "fail integer" do
  f ""      `shouldSatisfy` errorPosition 1 1
  f " 123"  `shouldSatisfy` errorPosition 1 1
  f " -123" `shouldSatisfy` errorPosition 1 1

integerBackward :: (Show a, Show b, Eq a, Eq b, Num t, Num a, IsString b) => (t -> IO (a, b)) -> SpecWith ()
integerBackward b = it "integer" do
  b 0      `shouldReturn` (0,    "0")
  b 1      `shouldReturn` (1,    "1")
  b 123    `shouldReturn` (123,  "123")
  b (-1)   `shouldReturn` (-1,   "-1")
  b (-123) `shouldReturn` (-123, "-123")

realForward :: (Show a, Show text, Eq a, Eq text, Monoid text, IsString t, IsString text, Fractional a) => (t -> EESP () text (a, Position () text)) -> Spec
realForward f = do
  naturalsForward f

  it "decimal" do
    f "1.0"     `shouldBe` Right (1.0,     Position () 1 4 mempty)
    f "123.123" `shouldBe` Right (123.123, Position () 1 8 mempty)

  it "negative" do
    f "-1.0"     `shouldBe` Right (-1.0,     Position () 1 5 mempty)
    f "-123.123" `shouldBe` Right (-123.123, Position () 1 9 mempty)

  describe "fail" do
    failIntegerForward f

    it "fail real" do
      f "1." `shouldSatisfy` errorPosition' (Just "Could not parse \"1.\" to a base 10 real.") 1 1

realBackward :: (Show a, Show b, Eq a, Eq b, IsString b, Fractional t, Fractional a) => (t -> IO (a, b)) -> Spec
realBackward b = do
  it "whole" do
    b 1   `shouldReturn` (1,   "1.0")
    b 123 `shouldReturn` (123, "123.0")

  it "decimal" do
    b (1.23)   `shouldReturn` (1.23,   "1.23")
    b (123.45) `shouldReturn` (123.45, "123.45")

  it "negative" do
    b (-1)      `shouldReturn` (-1,      "-1.0")
    b (-123)    `shouldReturn` (-123,    "-123.0")
    b (-1.23)   `shouldReturn` (-1.23,   "-1.23")
    b (-123.45) `shouldReturn` (-123.45, "-123.45")

newtype HexWord8 = HexWord8 Word deriving (Eq, Bits, Num, Integral, Real, Enum, Ord)
instance Show HexWord8 where show = ("0x" <>) . ($ mempty) . showHex

