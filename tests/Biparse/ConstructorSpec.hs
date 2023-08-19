module Biparse.ConstructorSpec where

import Biparse.Constructor as BC
--import GHC.Num ((+))
--import Control.Lens (_1, _2, (^.))

import Control.Monad.Reader (runReader)
import Control.Monad.State (runState)

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

  describe "expose" do
    let e = expose :: Constructor s Identity Identity u s

    it "forward" $ runReader (unFwd . pfst . deconstruct $ e) 'c' `shouldBe` 'c'

    it "backward" $ runState (($ ()) . unBwd . psnd . deconstruct $ e) 'c' `shouldBe` ('c','c')

