module Control.Monad.StateErrorSpec where

import Control.Monad.StateError

spec :: Spec
spec = do
  describe "Alternative" do
    it "empty" $ limit $
      runSET @() (fail "" :: StateErrorT 'ErrorStateInstance (Position () Text) (Either (ErrorState String (Position () Text))) ()) "abc" `shouldSatisfy` errorPosition 1 1

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
      runSET @() x (Identity 1) `shouldBe` Left (ErrorState "2" (Identity 2))

instance ChangeMonad () (Either (ErrorState String Int)) (Either String) where
  changeMonad' = first
type instance ChangeFunction () (Either (ErrorState String Int)) (Either String) = ErrorState String Int -> String

instance WrapErrorWithState String Int String where
  type StateForError String Int String = ()
  wrapErrorWithState' = const
  stateForError = const ()

