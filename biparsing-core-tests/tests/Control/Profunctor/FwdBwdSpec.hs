{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Control.Profunctor.FwdBwdSpec where

import Control.Profunctor.FwdBwd
import Control.Monad ((>>))

spec :: Spec
spec = do
  describe "Applicative and MonadFail" do
    let x :: MonadFail m => m ()
        x = fail "" *> x
        y :: MonadFail m => m ()
        y = fail "" >> y

    it ":*: Applicative" $ (x :: (ConstM Maybe :*: ConstM Maybe) () ()) `shouldBe` (ConstM Nothing :*: ConstM Nothing)
    it ":*: Monad" $ (y :: (ConstM Maybe :*: ConstM Maybe) () ()) `shouldBe` (ConstM Nothing :*: ConstM Nothing)
    it "FwdBwd" $ (x :: FwdBwd Maybe Maybe () ()) `shouldBe` (Fwd Nothing :*: Bwd \() -> Nothing)
    it "Fwd" $ unFwd x `shouldBe` Nothing
    it "Bwd" $ unBwd x () `shouldBe` Nothing

newtype ConstM m p a = ConstM (m a) deriving (Show, Eq, Functor, Applicative, Monad, MonadFail)
