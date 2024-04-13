module Control.Monad.FileMSpec where

import Prelude hiding (shouldBe)
import Control.Monad.FileM
import System.IO (openTempFile, openBinaryTempFile, hSeek, SeekMode(AbsoluteSeek))
import System.IO qualified as S
import Control.Monad.Writer.Class
import Test.Hspec.Expectations.Lifted (shouldBe)
import Data.ByteString.Lazy qualified as BL

spec :: Spec
spec = do
  describe "MonadWriter" do
    describe "String" do
      it "writer" do
        (_, h) <- openTempFile "/tmp" ""
        x <- runFileM @String h do
          tell "test "
          writer ('x', "writer")
        x `shouldBe` 'x'
        hSeek h AbsoluteSeek 0
        w <- S.hGetContents' h
        w `shouldBe` "test writer"
      
      describe "listen" do
        it "non-nested" do
          pendingWith "not implemented"
          (_, h) <- openTempFile "/tmp" ""
          x <- runFileM @String h do
            tell "test "
            (x,w) <- listen do
              tell "non-nested "
              writer ('1', "listen")
            x `shouldBe` '1'
            w `shouldBe` "non-nested listen"
            return '0'
          x `shouldBe` '0'
          hSeek h AbsoluteSeek 0
          w <- S.hGetContents' h
          w `shouldBe` "test listen"

        it "nested" do
          pendingWith "not implemented"
          (_, h) <- openTempFile "/tmp" ""
          x <- runFileM @String h do
            tell "test"
            (x,w) <- listen do
              tell " "
              (x,w) <- listen (writer ('2',"listen"))
              x `shouldBe` '2'
              w `shouldBe` "listen"
              return '1'
            x `shouldBe` '1'
            w `shouldBe` " listen"
            return '0'
          x `shouldBe` '0'
          hSeek h AbsoluteSeek 0
          w <- S.hGetContents' h
          w `shouldBe` "test listen"

      it "pass" do
        pendingWith "not implemented"

  describe "MonadWriter" do
    describe "LazyByteString" do
      it "writer" do
        (_, h) <- openBinaryTempFile "/tmp" ""
        x <- runFileM @LazyByteString h do
          tell "test "
          writer ('x', "writer")
        x `shouldBe` 'x'
        hSeek h AbsoluteSeek 0
        w <- BL.hGetContents h
        w `shouldBe` "test writer"
      
      describe "listen" do
        it "non-nested" do
          (_, h) <- openBinaryTempFile "/tmp" ""
          x <- runFileM @LazyByteString h do
            tell "test "
            (x,w) <- listen do
              tell "non-nested "
              writer ('1', "listen")
            x `shouldBe` '1'
            w `shouldBe` "non-nested listen"
            return '0'
          x `shouldBe` '0'
          hSeek h AbsoluteSeek 0
          w <- BL.hGetContents h
          w `shouldBe` "test non-nested listen"

        it "nested" do
          (_, h) <- openBinaryTempFile "/tmp" ""
          x <- runFileM @LazyByteString h do
            tell "test"
            (x,w) <- listen do
              tell " "
              (x,w) <- listen (writer ('2',"listen"))
              x `shouldBe` '2'
              w `shouldBe` "listen"
              return '1'
            x `shouldBe` '1'
            w `shouldBe` " listen"
            return '0'
          x `shouldBe` '0'
          hSeek h AbsoluteSeek 0
          w <- BL.hGetContents h
          w `shouldBe` "test listen"

      describe "pass" do
        it "non-nested" do
          (_, h) <- openBinaryTempFile "/tmp" ""
          x <- runFileM @LazyByteString h do
            tell "test "
            y <- pass do
              tell " detsen-non"
              return ('0', reverse)
            y `shouldBe` '0'
            writer ('1', "pass")
          x `shouldBe` '1'
          hSeek h AbsoluteSeek 0
          w <- BL.hGetContents h
          w `shouldBe` "test non-nested pass"

        it "is nested" do
          (_, h) <- openBinaryTempFile "/tmp" ""
          x <- runFileM @LazyByteString h do
            tell "test "
            y <- pass do
              tell " detsen"
              z <- pass do
                tell "is "
                return ('2', reverse)
              z `shouldBe` '2'
              return ('0', reverse)
            y `shouldBe` '0'
            writer ('1', "pass")
          x `shouldBe` '1'
          hSeek h AbsoluteSeek 0
          w <- BL.hGetContents h
          w `shouldBe` "test is nested pass"

