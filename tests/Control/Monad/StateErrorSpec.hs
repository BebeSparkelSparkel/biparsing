module Control.Monad.StateErrorSpec where

import Control.Monad.StateError
import Control.Monad.ChangeMonad
import Biparse.Error.WrapError

spec :: Spec
spec = do
  describe "Alternative" do
    it "empty" $ limit $
      runSET @() (empty :: StateErrorT 'ErrorStateInstance (Position Text) (Either (ErrorState String (Position Text))) ()) "abc" `shouldSatisfy` errorPosition 1 1

  describe "MonadError" do
    it "catch state is the last state before fail" $ limit do
      let x :: StateErrorT 'ErrorStateInstance Int (Either (ErrorState String Int)) Char
          x = catchError
            (do
              put 2
              throwError "3"
            )
            \_ -> do
              s <- get
              throwError $ show s
      runSET @() x 1 `shouldBe` Left "2"

instance ChangeMonad () (Either (ErrorState String Int)) (Either String) where
  type ChangeFunction () (Either (ErrorState String Int)) (Either String) =
    ErrorState String Int -> String
  changeMonad = first

instance WrapError String Int where
  type Error String Int = String
  type StateForError String Int = ()
  wrapError' = const
  stateForError = const ()

