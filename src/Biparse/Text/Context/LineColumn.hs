{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Biparse.Text.Context.LineColumn
  ( LineColumn
  , LinesOnly
  , Position(..)
  , startLineColumn
  , ErrorPosition(..)
  ) where

import Control.Monad.StateError (StateErrorT(StateErrorT), WrapError(Error,SubError,wrapError))
import Biparse.Biparser (SubState, GetSubState(getSubState), UpdateStateWithElement(updateElementContext), UpdateStateWithSubState(updateSubStateContext), ReplaceSubState(replaceSubState))
import GHC.Exts (IsList(Item))
import GHC.Exts qualified as GE

import Control.Monad.Trans.Error qualified as E

-- * Tracks line and column position

data LineColumn
data LinesOnly

data Position text = Position
  { line :: Int
  , column :: Int
  , subState :: text
  } deriving (Show, Eq)

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
  | NoPosition String
  deriving (Show, Eq)

instance
  ( MonadError (Error c (Position text) m) m
  , WrapError c (Position text) m
  , ErrorPosition ~ SubError c (Position text) m
  ) => MonadFail (StateErrorT c (Position text) m) where
  fail x = StateErrorT do
    s@(Position l c _) <- get
    throwError $ wrapError @c @_ @m (ErrorPosition l c x,s)

deriving instance MonadError ErrorPosition m => MonadError ErrorPosition (StateErrorT LinesOnly (Position text) m)
deriving instance MonadError ErrorPosition m => MonadError ErrorPosition (StateErrorT LineColumn (Position text) m)

instance WrapError LineColumn (Position text) (m ErrorPosition) where
  type Error    LineColumn (Position text) (m ErrorPosition) = ErrorPosition
  type SubError LineColumn (Position text) (m ErrorPosition) = ErrorPosition
  wrapError = \case
    (NoPosition e, Position l c _) -> ErrorPosition l c e
    (x, _) -> x

instance WrapError LinesOnly (Position text) (m ErrorPosition) where
  type Error    LinesOnly (Position text) (m ErrorPosition) = ErrorPosition
  type SubError LinesOnly (Position text) (m ErrorPosition) = ErrorPosition
  wrapError = \case
    (NoPosition e, Position l c _) -> ErrorPosition l c e
    (x, _) -> x

instance WrapError LineColumn s Identity where
  type Error LineColumn s Identity = Void
  type SubError LineColumn s Identity = Void
  wrapError = fst

instance E.Error ErrorPosition where strMsg = NoPosition

