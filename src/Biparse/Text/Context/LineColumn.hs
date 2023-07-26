{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Biparse.Text.Context.LineColumn
  ( LineColumn
  , LinesOnly
  , Position(..)
  , startLineColumn
  , ErrorPosition(..)
  , EEP
  , EESP
  , ElementToList
  , ListToElement
  ) where

import Biparse.Error.WrapError (WrapError(Error,StateForError,wrapError',stateForError))
import Biparse.Biparser (SubState, GetSubState(getSubState), UpdateStateWithElement(updateElementContext), UpdateStateWithSubState(updateSubStateContext), ReplaceSubState(replaceSubState))
import Control.Monad.StateError (ErrorState, ErrorContext, ErrorInstance(ErrorStateInstance))
import GHC.Exts (IsList(Item))
import GHC.Exts qualified as GE
import Control.Monad.ChangeMonad (ChangeMonad(ChangeFunction,changeMonad), ResultMonad(ResultingMonad,resultMonad))

import GHC.Err (undefined)
import Control.Monad.Trans.Error qualified as E

-- * Tracks line and column position

data LineColumn
data LinesOnly

data Position text = Position
  { line :: Int
  , column :: Int
  , subState :: text
  } deriving (Show, Eq, Functor)

type instance SubState LineColumn (Position text) = text
type instance SubState LinesOnly (Position text) = text

instance GetSubState LineColumn (Position text) where getSubState = subState
instance GetSubState LinesOnly (Position text) where getSubState = subState

instance Element text ~ Char => UpdateStateWithElement LineColumn (Position text) where
  updateElementContext  s@(Position {line,column}) c ss = case c of
    '\n' -> s {line = line + 1, column = 1, subState = ss}
    _ -> s {column = column + 1, subState = ss}

instance UpdateStateWithElement LinesOnly (Position [text]) where
  updateElementContext (Position {line}) _ ss' =
    Position {line = line + 1, column = 1, subState = ss'}

instance (Element text ~ Char, MonoFoldable text) => UpdateStateWithSubState LineColumn (Position text) where
  updateSubStateContext s@(Position {line, column}) ss ss' = if ns == 0
    then s {column = column + cs, subState = ss'}
    else s {line = line + ns, column = cs, subState = ss'}
    where
    (ns, cs) = flip execState (0, 0) $ for_ ss \case
      '\n' -> modify \(l,_) -> (l + 1, 1)
      _ -> modify $ second (+ 1)

instance MonoFoldable text => UpdateStateWithSubState LinesOnly (Position text) where
  updateSubStateContext s@(Position {column}) ss ss' =
    s {column = column + length ss, subState = ss'}

instance ReplaceSubState (Position a) ss (Position ss) where
  replaceSubState p ss = p {subState = ss}

startLineColumn :: text -> Position text
startLineColumn = Position 1 1

instance IsString text => IsString (Position text) where
  fromString = startLineColumn . fromString

instance IsList text => IsList (Position text) where
  type Item (Position text) = Item text
  fromList = startLineColumn . GE.fromList
  toList = GE.toList . subState

-- * Positional Errors

data ErrorPosition
  = ErrorPosition Int Int String
  deriving (Show, Eq)

instance E.Error ErrorPosition where strMsg = undefined
instance E.Error (ErrorState String (Position text)) where strMsg = undefined

instance WrapError String (Position text) where
  type Error String (Position text) = ErrorPosition
  type StateForError String (Position text) = Position text
  wrapError' msg (Position l c _) = ErrorPosition l c msg
  stateForError = id

instance ResultMonad (Either ErrorPosition) () where
  type ResultingMonad (Either ErrorPosition) () = Either ErrorPosition
  resultMonad = ()

type EEP e ss = Either (ErrorState e (Position ss))
type EESP ss = EEP String ss

instance ChangeMonad () (EESP text) (Either ErrorPosition) where
  type ChangeFunction () (EESP text) (Either ErrorPosition) =
    ErrorState String (Position text) -> ErrorPosition
  changeMonad = first

-- | "This instance is not sound and is a hack for zoom. The monad conversion in zoom should be more complete or throw away the text entirely but 'catch' in 'MonadError e (StateErrorT s m)' makes this difficult.
data ElementToList
instance ChangeMonad ElementToList (EEP e text) (EEP e [text]) where
  type ChangeFunction ElementToList (EEP e text) (EEP e [text]) = ()
  changeMonad () = first $ second $ fmap $ singleton

data ListToElement
instance Monoid text => ChangeMonad ListToElement (EEP e [text]) (EEP e text) where
  type ChangeFunction ListToElement (EEP e [text]) (EEP e text) = ()
  changeMonad () = first $ second $ ($> mempty)
  
type instance ErrorContext LineColumn = 'ErrorStateInstance
type instance ErrorContext LinesOnly = 'ErrorStateInstance

