{-# LANGUAGE NamedFieldPuns #-}
module Biparse.Text.PositionContext
  ( LineColumn
  , Position(..)
  , startPosition
  ) where

import Biparse.Biparser (SubState, GetSubState(getSubState), UpdateStateWithElement(updateElementContext), UpdateStateWithSubState(updateSubStateContext))

-- * Tracks line and column

data LineColumn

data Position text = Position
  { line :: Int
  , column :: Int
  , subState :: text
  } deriving (Show, Eq)

type instance SubState LineColumn (Position text) = text

instance GetSubState LineColumn (Position text) where
  getSubState (Position {subState}) = subState

instance Element text ~ Char => UpdateStateWithElement LineColumn (Position text) where
  updateElementContext s@(Position {line,column}) c ss = case c of
    '\n' -> s {line = line + 1, column = 1, subState = ss}
    _ -> s {column = column + 1, subState = ss}

instance (Element text ~ Char, MonoFoldable text) => UpdateStateWithSubState LineColumn (Position text) where
  updateSubStateContext s ss ss' = if ns == 0
    then s {column = column s + cs, subState = ss'}
    else s {line = line s + ns, column = cs, subState = ss'}
    where
    (ns, cs) = flip execState (0, 0) $ for_ ss \case
      '\n' -> modify \(l,_) -> (l + 1, 1)
      _ -> modify $ second (+ 1)

startPosition :: text -> Position text
startPosition = Position 1 1

instance IsString text => IsString (Position text) where
  fromString = startPosition . fromString

