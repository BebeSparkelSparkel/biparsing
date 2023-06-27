module Biparse.Text.NumericSpec where

import Biparse.Text.Numeric

spec :: Spec
spec = do
  fb "intBaseTen"
    (intBaseTen :: Iso LineColumn IO IO (Position Text) Int)
    (\f -> do
      it "success" do
        f "1" >>= (`shouldBe` (1, Position 1 2 mempty))
        f "123" >>= (`shouldBe` (123, Position 1 4 mempty))
        f "123x" >>= (`shouldBe` (123, Position 1 4 "x"))

      it "fail" do
        f "" `shouldThrow` isUserError
        f " 123" `shouldThrow` isUserError
    )
    \b -> do
      it "success" do
        b 1 >>= (`shouldBe` (1, "1"))
        b 123 >>= (`shouldBe` (123, "123"))

