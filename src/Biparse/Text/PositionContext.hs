{-# LANGUAGE NamedFieldPuns #-}
module Biparse.Text.PositionContext
  ( LineColumn
  , LinesOnly
  , Position(..)
  , startLineColumn
  ) where

import Biparse.Biparser (SubState, GetSubState(getSubState), UpdateStateWithElement(updateElementContext), UpdateStateWithSubState(updateSubStateContext), ReplaceSubState(replaceSubState))
import GHC.Exts (IsList(Item))
import GHC.Exts qualified as GE

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

-- $ Tracks column position
--data Column
--
--data ColumnPosition text = ColumnPosition
--  { column :: Int
--  , subState :: text
--  } deriving (Show, Eq)
--
--startColumn :: text -> ColumnPosition text
--startColumn = ColumnPosition 1
--
--instance IsString text => IsString (ColumnPosition text) where
--  fromString = startColumn . fromString

