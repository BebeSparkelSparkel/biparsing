module Biparse.FileSpec where

import Biparse.File
import System.IO
import Biparse.Text.Numeric

spec :: Spec
spec = do
  describe "BinaryFile" do
    it "decodeBinaryFile" do
      (fp,h) <- openBinaryTempFile "/tmp" ""
      hPutStr h "1234"
      hClose h
      result <- decodeBinaryFile fp naturalBaseTen
      result `shouldBe` (1234 :: Int, BinaryPosition fp 4 "")

    it "encodeToBinaryFile" do
      (fp,h) <- openTempFile "/tmp" ""
      hClose h
      x <- encodeToBinaryFile fp () () (4321 :: Int) naturalBaseTen
      x `shouldBe`  (4321, ())
      readFile fp `shouldReturn` "4321"

  describe "TextFile" do
    it "decodeTextFile" do
      (fp,h) <- openTempFile "/tmp" ""
      hPutStr h "1234"
      hClose h
      result <- decodeTextFile fp naturalBaseTen
      result `shouldBe` (1234 :: Int, TextPosition fp 1 5 "")

    it "encodeToTextFile" do
      (fp,h) <- openTempFile "/tmp" ""
      hClose h
      x <- encodeToTextFile fp () () (4321 :: Int) naturalBaseTen
      x `shouldBe`  (4321, ())
      readFile fp `shouldReturn` "4321"

