{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Biparse.Context.Index
  ( IndexContext
  , IndexPosition(..)
  , ErrorIndex(..)
  , EIP
  , EISP
  ) where

import Control.Monad.StateError (ErrorContext, ErrorState)
import Biparse.Error.WrapError (WrapError(Error,StateForError,wrapError',stateForError))
import Biparse.Biparser (SubState, GetSubState(getSubState), UpdateStateWithElement(updateElementContext), UpdateStateWithSubState(updateSubStateContext))
import Control.Monad.StateError (ErrorInstance(ErrorStateInstance))
import GHC.Exts (IsList(Item))
import GHC.Exts qualified as GE
import Control.Monad.ChangeMonad (ChangeMonad(ChangeFunction,changeMonad'), ResultMonad(ResultingMonad,resultMonad))

import GHC.Err (undefined)
import Control.Monad.Trans.Error qualified as E

-- * Identity Context
-- Use as the context if @state ~ SubState IdentityState state@ basically if there is no context outside the 

data IndexContext

data IndexPosition ss = IndexPosition
  { index :: Index ss
  , subState :: ss
  }
deriving instance (Show (Index ss), Show ss) => Show (IndexPosition ss)
deriving instance (Eq (Index ss), Eq ss) => Eq (IndexPosition ss)

type instance SubState IndexContext (IndexPosition ss) = ss
instance GetSubState IndexContext (IndexPosition ss) where getSubState = subState

instance IsSequence ss => UpdateStateWithSubState IndexContext (IndexPosition ss) where
  updateSubStateContext (IndexPosition i _) consumed remaining = IndexPosition (i + lengthIndex consumed) remaining

instance Num (Index ss) => UpdateStateWithElement IndexContext (IndexPosition ss) where
  updateElementContext (IndexPosition i _) _ ss = IndexPosition (i + 1) ss

type instance ErrorContext IndexContext = 'ErrorStateInstance

startIndex :: Num (Index ss) => ss -> IndexPosition ss
startIndex = IndexPosition 0

instance (IsString text, Num (Index text)) => IsString (IndexPosition text) where
  fromString = startIndex . fromString

instance (IsList ss, Num (Index ss)) => IsList (IndexPosition ss) where
  type Item (IndexPosition ss) = Item ss
  fromList = startIndex . GE.fromList
  toList = GE.toList . subState

-- * Positional Errors

data ErrorIndex ss = ErrorIndex (Index ss) String
deriving instance Show (Index ss) => Show (ErrorIndex ss)
deriving instance Eq (Index ss) => Eq (ErrorIndex ss)

instance E.Error (ErrorIndex ss) where strMsg = undefined
instance E.Error (ErrorState String (IndexPosition text)) where strMsg = undefined

instance WrapError String (IndexPosition ss) where
  type Error String (IndexPosition ss) = ErrorIndex ss
  type StateForError String (IndexPosition ss) = Index ss
  wrapError' msg i = ErrorIndex i msg
  stateForError = index

instance ResultMonad (Either (ErrorIndex ss)) () where
  type ResultingMonad (Either (ErrorIndex ss)) () = Either (ErrorIndex ss)
  resultMonad = ()

type EIP e ss = Either (ErrorState e (IndexPosition ss))
type EISP ss = EIP String ss

instance ChangeMonad () (Either (ErrorState e (IndexPosition ss))) (Either (ErrorIndex ss)) where
  type ChangeFunction () (Either (ErrorState e (IndexPosition ss))) (Either (ErrorIndex ss)) =
    ErrorState e (IndexPosition ss) -> ErrorIndex ss
  changeMonad' = first
