module Control.Monad.UnrecoverableSpec where

import Control.Monad.Unrecoverable
import Data.Ord (Ordering(LT,EQ,GT), compare)
import System.IO.Error (userError, ioeGetErrorString)

spec :: Spec
spec = do
  it "throws unrecoverable error" $
    evalUnrecoverableT (throwUnrecoverable $ userError "unrecoverable error") `shouldThrow` (== userError "unrecoverable error")

  it "still works with monad fail" $
    evalUnrecoverableT (fail "fail") `shouldThrow` (== userError "fail")

  it "does not try alternatives after unrecoverable" $
    evalUnrecoverableT (throwUnrecoverable (userError "unrecoverable error") <|> pure ()) `shouldThrow` (== userError "unrecoverable error")

  it "does try alternatives after recoverable" $
    (evalUnrecoverableT (empty <|> pure ()) :: IO ())

  it "with IO" do
    let f :: Int -> IO Int
        f x = evalUnrecoverableT do
          y <- pure 2
          ((case compare x y of
            LT -> throwUnrecoverable $ userError "Less than 2"
            EQ -> fail "equal to 2"
            GT -> pure x)
            <|> pure 0)
    f 1 `shouldThrow` \e -> isUserError e && ((== "Less than 2") $ ioeGetErrorString e)
    f 2 >>= (`shouldBe` 0)
    f 3 >>= (`shouldBe` 3)

