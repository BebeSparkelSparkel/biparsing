{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverloadedStrings #-}
module Biparse.Text.LineBreak
  ( LineBreakType(..)
  , lines
  , lineBreakType
  , LineBreaker
  , LineBreakerString(lineBreakerString)
  , UpdateSuperState
  , LineSplitter(..)
  ) where

import Biparse.Biparser (Iso, SubState, SubElement, SubStateContext, ElementContext)
import Biparse.General (takeDi, takeDi', Take, Take')
import Biparse.List (splitElem, splitOn)
import Biparse.Utils (char)
import Data.Char (Char)

data LineBreakType
  = Unix
  | Windows
  deriving (Show, Eq, Ord)

type LineBreaker :: LineBreakType -> Either Char Symbol
type family LineBreaker a where
  LineBreaker 'Unix = 'Left '\n'
  LineBreaker 'Windows = 'Right "\r\n"

class LineBreakerString (lb :: LineBreakType) where lineBreakerString :: IsString a => a
instance LineBreakerString 'Unix where lineBreakerString = "\n"
instance LineBreakerString 'Windows where lineBreakerString = "\r\n"

lineBreakType :: forall c m n a text se.
  ( Take c a m n text se
  , Take' c a m n text se
  , IsChar se
  , Alternative n
  , IsString text
  ) => Iso c m n a LineBreakType
lineBreakType
  =   takeDi  (fromChar '\n') Unix
  <|> takeDi' "\r\n"          Windows

lines :: forall (lb :: LineBreakType) c m n a text up.
  ( LineSplitter (LineBreaker lb) up c m n a
  , text ~ SubState c a
  , up ~ UpdateSuperState c
  )
  => Iso c m n a [text]
lines = lineSplitter @(LineBreaker lb) @(UpdateSuperState c)

-- | Used to indicate if 'updateElementContext' and 'updateSubStateContext' should be used. An optimization since 'lines' knows how to update the context without traverseing the consumed substate.
type UpdateSuperState :: Type -> Bool
type family UpdateSuperState a

class LineSplitter (lb :: Either Char Symbol) (up :: Bool) c m n a where lineSplitter :: Iso c m n a [SubState c a]
instance
  ( MonadState a m
  , MonadWriter ss n
  , IsChar se
  , KnownChar char
  , IsSequence ss
  , Eq se
  , Show se
  , ElementContext c a
  , ss ~ SubState c a
  , se ~ SubElement c a
  ) => LineSplitter ('Left char) 'True c m n a where
  lineSplitter = splitElem @c @a @m @n (char @char)
instance
  ( MonadState a m
  , MonadWriter ss n
  , KnownSymbol sym
  , EqElement ss
  , IsString ss
  , Show ss
  , SubStateContext c a
  , ElementContext c a
  , ss ~ SubState c a
  , se ~ SubElement c a
  ) => LineSplitter ('Right sym) 'True c m n a where
  lineSplitter = splitOn (symbol @sym)

