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
          , LineSplitter (LineBreaker lb) (UpdateSuperState (LineColumn lb)) (LineColumn lb) (StateErrorT 'ErrorStateInstance (Position () text) (FM text)) (RWST () text () EitherString) (Position () text)
          )
          => String
          -> Spec
        test = flip it let
          f = runForward @() @(LineColumn lb) @_ @(FM text) @_ @EitherString @() @() $ lines @lb
          text = startLineColumn $ (repeatConcat numLines $ line $ lineBreakerString @lb :: text)
          in seq text $ limit $ f text `shouldSatisfy` isRight

    test @'Windows @String "Windows String"
    test @'Windows @ByteString "Windows ByteString"
    test @'Windows @Text "Windows Text"
    --test @'Windows @(Seq Char) "Windows (Seq Char)"
    test @'Unix @String "Unix String"
    test @'Unix @ByteString "Unix ByteString"
    test @'Unix @Text "Unix Text"
    test @'Unix @(Seq Char) "Unix (Seq Char)"

repeatConcat :: Monoid m => Natural -> m -> m
repeatConcat = \case
  0 -> const mempty
  x -> \y -> y <> repeatConcat (x - 1) y

