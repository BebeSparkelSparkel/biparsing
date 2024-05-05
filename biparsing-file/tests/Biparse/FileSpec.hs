module Biparse.FileSpec where

import Biparse.File
import System.IO
import Biparse.Text.Numeric

spec :: Spec
spec = do
  describe "BinaryFile" do
    it "decodeFile" do
      (fp,h) <- openBinaryTempFile "/tmp" ""
      hPutStr h "1234"
      hClose h
      n <- decodeBinaryFile fp naturalBaseTen
      n `shouldBe` (1234 :: Int)

