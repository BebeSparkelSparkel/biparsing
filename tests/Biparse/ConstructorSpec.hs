module Biparse.ConstructorSpec where

import Biparse.Constructor as BC
--import GHC.Num ((+))
--import Control.Lens (_1, _2, (^.))

import Control.Monad.Reader (runReaderT)
import Control.Lens (_1)

spec :: Spec
spec = do
  --describe "focusOne" do
  --  fb @() "one"
  --    (focusOne id do
  --      expectFwd _1 1
  --      x <- exposes (^. _2)
  --      return $ x + 1
  --    :: Biparser IdentityState [(Int,Double)] EitherString EitherString [(Int,Double)] Double)
  --    (\f -> do
  --      it "empty" $ f empty `shouldSatisfy` isString

  --      it "success" $ f [(1,2)] `shouldBe` EValue (3,mempty)

  --      it "expect fail" $ f [(2,3)] `shouldSatisfy` isString
  --    )
  --    \b -> do
  --      it "empty" $ b empty `shouldSatisfy` isString

  --      it "success" $ b [(1,3)] `shouldBe` EValue (4,[(1,3)])

  describe "lensBiparse" do
    let lb = lensBiparse _1 (take 1 :: Biparser IdentityState [Int] EitherString EitherString () ()) :: Constructor ([Int],()) EitherString EitherString () ()

    describe "forward" do
      let f = runForwardC lb
      it "success" $ f ([1],()) `shouldBe` EValue ()
      it "fail" $ f ([1],()) `shouldBe` EValue ()

    describe "backward" do
      let b = runBackwardC lb ()
      it "success" $ b ([],()) `shouldBe` EValue ((), ([1], ()))

  describe "expose" do
    let e = expose :: Constructor s Identity Identity u s

    it "forward" $ runForwardC e 'c' `shouldBe` Identity 'c'

    it "backward" $ runBackwardC e () 'c' `shouldBe` Identity ('c','c')

runForwardC :: Constructor s m n u v -> s -> m v
runForwardC c s = runReaderT (unFwd $ pfst $ deconstruct c) s

runBackwardC :: Constructor s m n u v -> u -> s -> n (v,s)
runBackwardC c u s = runStateT ((unBwd $ psnd $ deconstruct c) u) s

