module Biparse.PolarizeSpec where

--import Biparse.Polarize
--import Biparse.Text.LineBreak (lines, LineBreakType(Unix))
--import Biparse.List (all, takeElementsWhile)
--import Data.Sequences qualified as MT

spec :: Spec
spec = pure ()
  --describe "polarize" do

  --  fb "Biparser success"
  --    --(polarize @UnixLC @UnixLC @UnixLC @'(StateErrorT,Either) @(N UnixLC EitherString () [String] ())
  --    (polarize @UnixLC @ColumnsOnly @UnixLC @'(StateErrorT,Either)
  --      (lines @'Unix)
  --      (all $ MT.reverse <$> one)
  --    :: Iso UnixLC (FM String) EitherString () String () (Position () String) [String])
  --    ()
  --    ()
  --    (\f -> do
  --      it "success" $ f "abc\ndef\nghi" `shouldBe` Right (["cba","fed","ihg"], Position () 3 4 mempty)
  --    )
  --    \b -> do
  --      it "success" $ b ["cba","fed","ihg"] `shouldBe` EValue (["abc","def","ghi"], "cba\nfed\nihg")

