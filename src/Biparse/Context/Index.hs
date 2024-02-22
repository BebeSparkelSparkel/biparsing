{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Biparse.Context.Index
  ( IndexContext
  , IndexPosition(..)
  , ErrorIndex(..)
  , EIP
  , EISP
  ) where

import Control.Monad.StateError (ErrorContext, ErrorState(ErrorState), WrapErrorWithState(StateForError,wrapErrorWithState',stateForError))
import Biparse.Biparser (SubState, GetSubState(getSubState), UpdateStateWithElement(updateElementContext), UpdateStateWithSubState(updateSubStateContext))
import Control.Monad.StateError (ErrorInstance(ErrorStateInstance))
import GHC.Exts (IsList(Item))
import GHC.Exts qualified as GE
import Control.Monad.ChangeMonad (ChangeMonad, ChangeFunction, changeMonad', ResultMonad(ResultingMonad,resultMonad))

--import Control.Monad.Trans.Error qualified as E

-- * Identity Context
-- Use as the context if @state ~ SubState state@ basically if there is no context outside the 

data IndexContext

data IndexPosition ss = IndexPosition
  { index :: Index ss
  , subState :: ss
  }
deriving instance (Show (Index ss), Show ss) => Show (IndexPosition ss)
deriving instance (Eq (Index ss), Eq ss) => Eq (IndexPosition ss)

instance GetSubState (IndexPosition ss) where
  type SubState (IndexPosition ss) = ss
  getSubState = subState

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

instance WrapErrorWithState String (IndexPosition ss) (ErrorIndex ss) where
  type StateForError String (IndexPosition ss) (ErrorIndex ss) = Index ss
  wrapErrorWithState' msg i = ErrorIndex i msg
  stateForError = index

instance ResultMonad (Either (ErrorIndex ss)) () where
  type ResultingMonad (Either (ErrorIndex ss)) () = Either (ErrorIndex ss)
  resultMonad = ()

instance ResultMonad (EISP ss) () where
  type ResultingMonad (EISP ss) () = Either (ErrorIndex ss)
  resultMonad (ErrorState e (IndexPosition {index}))  = ErrorIndex index e

type EIP e ss = Either (ErrorState e (IndexPosition ss))
type EISP ss = EIP String ss

instance ChangeMonad () (EIP e ss) (Either (ErrorIndex ss)) where
  changeMonad' = first
type instance ChangeFunction () (EIP e ss) (Either (ErrorIndex ss)) = ErrorState e (IndexPosition ss) -> ErrorIndex ss

-- * Convert Instance Contexts

instance Applicative m => ConvertSequence IndexContext a a m where convertSequence = pure

instance (e ~ Element seq, MonoPointed seq, Applicative m) => ConvertElement IndexContext e seq m where convertElement = pure . singleton

