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

import Biparse.Error.WrapError (WrapError(Error,wrapError))
import Biparse.Biparser (SubState, GetSubState(getSubState), UpdateStateWithElement(updateElementContext), UpdateStateWithSubState(updateSubStateContext), ReplaceSubState(replaceSubState))
import Control.Monad.StateError (ErrorState(ErrorState))
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

instance E.Error ErrorPosition where strMsg = undefined

instance E.Error (ErrorState String (Position text)) where strMsg msg = undefined
--instance Monoid text => E.Error (ErrorState String (Position text)) where strMsg msg = ErrorState msg $ Position (-1) (-1) mempty

instance MonadFail (Either ErrorPosition) where fail = Left . NoPosition

instance WrapError ErrorPosition (Position text) where
  type Error ErrorPosition (Position text) = ErrorPosition
  wrapError e (Position l c _) = case e of
    NoPosition msg -> ErrorPosition l c msg
    _ -> undefined

instance WrapError String (Position text) where
  type Error String (Position text) = ErrorPosition
  wrapError msg (Position l c _) = ErrorPosition l c msg

instance ResultMonad (Either ErrorPosition) where
  type ResultingMonad (Either ErrorPosition) = Either ErrorPosition
  resultMonad = ()

instance ChangeMonad (Either (ErrorState String (Position text))) (Either ErrorPosition) where
  type ChangeFunction (Either (ErrorState String (Position text))) (Either ErrorPosition) =
    ErrorState String (Position text) -> ErrorPosition
  changeMonad = first

instance ChangeMonad (Either ErrorPosition) (Either ErrorPosition) where
  type ChangeFunction (Either ErrorPosition) (Either ErrorPosition) = ()
  changeMonad = const id

--instance ResultMonad (Either (ErrorState String (Position text))) where
--  type ResultingMonad (Either (ErrorState String (Position text))) = Either ErrorPosition
--  resultMonad = _
