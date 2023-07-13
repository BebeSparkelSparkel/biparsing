module Control.Monad.StateErrorSpec where

import Control.Monad.StateError

spec :: Spec
spec = do
  describe "Alternative" do
    it "empty" $ limit $
      runSET (empty :: StateErrorT LineColumn (Position Text) FM ()) "abc" `shouldSatisfy` errorPosition 1 1

