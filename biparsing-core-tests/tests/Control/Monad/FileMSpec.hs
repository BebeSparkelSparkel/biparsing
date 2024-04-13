{-# LANGUAGE AllowAmbiguousTypes #-}
module Control.Monad.FileMSpec where

import Prelude hiding (shouldBe)
import Control.Monad.FileM
import System.IO (openTempFile, openBinaryTempFile, hSeek, SeekMode(AbsoluteSeek))
import System.IO qualified as S
import Control.Monad.Writer.Class
import Test.Hspec.Expectations.Lifted (shouldBe)
import Data.ByteString.Lazy qualified as BL
import Type.Reflection (Typeable, typeRep)

spec :: Spec
spec = do
  describe "MonadWriter" do
    monadWriterTest @StrictByteString
    monadWriterTest @LazyByteString

    monadWriterIncompleteTest @String
    monadWriterIncompleteTest @StrictText
    monadWriterIncompleteTest @LazyText

monadWriterTest :: forall w.
  ( Typeable w
  , MonadWriter w (FileM w)
  , SemiSequence w
  , IsString w
  , Eq w
  , Show w
  ) => Spec
monadWriterTest =
  describe (show $ typeRep @w) do
    it "writer" do
      (_, h) <- openBinaryTempFile "/tmp" ""
      x <- runFileM @w h do
        tell "test "
        writer ('x', "writer")
      x `shouldBe` 'x'
      hSeek h AbsoluteSeek 0
      w <- BL.hGetContents h
      w `shouldBe` "test writer"
    
    describe "listen" do
      it "non-nested" do
        (_, h) <- openBinaryTempFile "/tmp" ""
        x <- runFileM @w h do
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
        x <- runFileM @w h do
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
        x <- runFileM @w h do
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
        x <- runFileM @w h do
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

monadWriterIncompleteTest :: forall w.
  ( Typeable w
  , IsString w
  , MonadWriter w (FileM w)
  ) => Spec
monadWriterIncompleteTest =
  describe (show $ typeRep @w) do
    it "writer" do
      (_, h) <- openTempFile "/tmp" ""
      x <- runFileM @w h do
        tell @w "test "
        writer ('x', "writer")
      x `shouldBe` 'x'
      hSeek h AbsoluteSeek 0
      w <- S.hGetContents' h
      w `shouldBe` "test writer"
    
    describe "listen" do
      it "non-nested" $ pendingWith "not implemented"

      it "nested" $ pendingWith "not implemented"

    it "pass" $ pendingWith "not implemented"

