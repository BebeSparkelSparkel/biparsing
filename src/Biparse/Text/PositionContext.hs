module Biparse.Text.PositionContext
  ( LineColumn
  , Position(..)
  ) where

import Data.MonoTraversable (Element)
import Data.Char (Char)
import Data.Int (Int)
import GHC.Num ((+))
import Data.Function (flip)
import Biparse.BiparserT (StateContext(setStateContext,setSubState,getSubState), SubState)
import Data.String (IsString(fromString))
import Text.Show (Show)
import Data.Function ((.))
import Data.Eq (Eq)

data LineColumn

data Position text = Position
  { row :: Int
  , column :: Int
  , subState :: text
  } deriving (Show, Eq)

type instance SubState LineColumn (Position text) = text

instance Element text ~ Char => StateContext LineColumn (Position text) where
  setStateContext s c ss = flip (setSubState @LineColumn) ss case c of
    '\n' -> s {row = row s + 1, column = 1}
    _ -> s {column = column s + 1}
  setSubState s ss = s {subState = ss}
  getSubState = subState

instance IsString text => IsString (Position text) where
  fromString = Position 1 1 . fromString
