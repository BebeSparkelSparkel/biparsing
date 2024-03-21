{-# LANGUAGE AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -Wno-monomorphism-restriction #-}
module Biparse.Text.LineBreakSpec where

import Biparse.Text.LineBreak
import GHC.Prim (seq)

spec :: Spec
spec = do
  describe "performance" do
    let
        numLines = 500
        line :: (IsString a, Semigroup a) => a -> a
        line = ("abcdefghijklmnopqrstuvwxyz" <>)
        test :: forall (lb :: LineBreakType) text .
          ( LineBreakerString lb
          , IsString text
          , Show text
          , Monoid text
          , LineSplitter (LineBreaker lb) (UpdateSuperState (LineColumn lb)) (LineColumn lb) (StateErrorT 'ErrorStateInstance (Position () text) (FM text)) (RWST () text () EitherString) (Position () text) [text]
          )
          => String
          -> Spec
        test = flip it let
          f = evalForward @() @(LineColumn lb) @_ @(FM text) @_ @EitherString @() @text @() @_ @[text] $ lines @lb
          text :: Position () text
          text = startLineColumn $ repeatConcat numLines $ line $ lineBreakerString @lb
          in seq text $ limit $ f text `shouldSatisfy` isRight

    test @'Windows @String "Windows String"
    test @'Windows @ByteString "Windows ByteString"
    test @'Windows @Text "Windows Text"
    test @'Windows @(Seq Char) "Windows (Seq Char)"
    test @'Unix @String "Unix String"
    test @'Unix @ByteString "Unix ByteString"
    test @'Unix @Text "Unix Text"
    test @'Unix @(Seq Char) "Unix (Seq Char)"

  describe "LineSplitter" do
    fb @() "LineSplitter ('Left '\\n') 'True UnixLC (FM ByteString) EitherString (Position () ByteString)"
      (lineSplitter @('Left '\n') @'True :: Iso UnixLC (FM ByteString) EitherString () ByteString () (Position () ByteString) [ByteString])
      ()
      ()
      (\f -> do
        it "empty" $ f "" `shouldBe` Right ([], Position () 1 1 "")
        it "no break" $ f "abc" `shouldBe` Right (["abc"], Position () 1 4 "")
        it "one break" $ f "abc\ndef" `shouldBe` Right (["abc","def"], Position () 2 4 "")
        it "two breaks" $ f "abc\ndef\n" `shouldBe` Right (["abc","def",""], Position () 3 1 "")
      )
      \b -> do
        it "empty" $ b [] `shouldBe` EValue ([], "")
        it "no break" $ b ["abc"] `shouldBe` EValue (["abc"], "abc")
        it "one break" $ b ["abc","def"] `shouldBe` EValue (["abc","def"], "abc\ndef")
        it "two breaks" $ b ["abc","def",""] `shouldBe` EValue (["abc","def",""], "abc\ndef\n")

    fb @() "LineSplitter ('Right '\\r\\n') 'True UnixLC (FM ByteString) EitherString (Position () ByteString)"
      (lineSplitter @('Right "\r\n") @'True :: Iso UnixLC (FM ByteString) EitherString () ByteString () (Position () ByteString) [ByteString])
      ()
      ()
      (\f -> do
        it "empty" $ f "" `shouldBe` Right ([], Position () 1 1 "")
        it "no break" $ f "abc" `shouldBe` Right (["abc"], Position () 1 4 "")
        it "one break" $ f "abc\r\ndef" `shouldBe` Right (["abc","def"], Position () 2 4 "")
        it "two breaks" $ f "abc\r\ndef\r\n" `shouldBe` Right (["abc","def",""], Position () 3 1 "")
      )
      \b -> do
        it "empty" $ b [] `shouldBe` EValue ([], "")
        it "no break" $ b ["abc"] `shouldBe` EValue (["abc"], "abc")
        it "one break" $ b ["abc","def"] `shouldBe` EValue (["abc","def"], "abc\r\ndef")
        it "two breaks" $ b ["abc","def",""] `shouldBe` EValue (["abc","def",""], "abc\r\ndef\r\n")

    fb @() "LineSplitter ('Left '\\n') 'False UnixLC (FM ByteString) EitherString (Position () ByteString)"
      (lineSplitter @('Left '\n') @'False :: Iso UnixLC (FM ByteString) EitherString () ByteString () (Position () ByteString) [ByteString])
      ()
      ()
      (\f -> do
        it "empty" $ f "" `shouldBe` Right ([], Position () 1 1 "")
        it "no break" $ f "abc" `shouldBe` Right (["abc"], Position () 1 1 "")
        it "one break" $ f "abc\ndef" `shouldBe` Right (["abc","def"], Position () 1 1 "")
        it "two breaks" $ f "abc\ndef\n" `shouldBe` Right (["abc","def",""], Position () 1 1 "")
      )
      \b -> do
        it "empty" $ b [] `shouldBe` EValue ([], "")
        it "no break" $ b ["abc"] `shouldBe` EValue (["abc"], "abc")
        it "one break" $ b ["abc","def"] `shouldBe` EValue (["abc","def"], "abc\ndef")
        it "two breaks" $ b ["abc","def",""] `shouldBe` EValue (["abc","def",""], "abc\ndef\n")

    fb @() "LineSplitter ('Right '\\r\\n') 'False UnixLC (FM ByteString) EitherString (Position () ByteString)"
      (lineSplitter @('Right "\r\n") @'False :: Iso UnixLC (FM ByteString) EitherString () ByteString () (Position () ByteString) [ByteString])
      ()
      ()
      (\f -> do
        it "empty" $ f "" `shouldBe` Right ([], Position () 1 1 "")
        it "no break" $ f "abc" `shouldBe` Right (["abc"], Position () 1 1 "")
        it "one break" $ f "abc\r\ndef" `shouldBe` Right (["abc","def"], Position () 1 1 "")
        it "two breaks" $ f "abc\r\ndef\r\n" `shouldBe` Right (["abc","def",""], Position () 1 1 "")
      )
      \b -> do
        it "empty" $ b [] `shouldBe` EValue ([], "")
        it "no break" $ b ["abc"] `shouldBe` EValue (["abc"], "abc")
        it "one break" $ b ["abc","def"] `shouldBe` EValue (["abc","def"], "abc\r\ndef")
        it "two breaks" $ b ["abc","def",""] `shouldBe` EValue (["abc","def",""], "abc\r\ndef\r\n")

repeatConcat :: Monoid m => Natural -> m -> m
repeatConcat = \case
  0 -> const mempty
  x -> \y -> y <> repeatConcat (x - 1) y

