module Control.Monad.StateErrorSpec where

import Control.Monad.StateError

spec :: Spec
spec = do
  describe "Alternative" do
    it "empty" $ limit $
      runStateErrorT (fail "" :: StateErrorT 'ErrorStateInstance (Position () Text) (EESP () Text) ()) "abc" `shouldSatisfy` errorPosition 1 1

  describe "MonadError" do
    it "catch state is the last state before fail" $ limit do
      let x :: StateErrorT 'ErrorStateInstance (Identity Int) (Either (ErrorState String (Identity Int))) Char
          x = catchError
            (do
              put 2
              throwError "3"
            )
            \_ -> do
              Identity s <- get
              throwError $ show s
      runStateErrorT x (Identity 1) `shouldBe` Left (ErrorState "2" (Identity 2))

instance ChangeMonad () (Either (ErrorState String Int)) (Either String) (ErrorState String Int -> String) where
  changeMonad' = first
--type instance ChangeFunction () (Either (ErrorState String Int)) (Either String) = ErrorState String Int -> String

