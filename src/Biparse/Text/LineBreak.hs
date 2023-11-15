{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverloadedStrings #-}
module Biparse.Text.LineBreak
  ( LineBreakType(..)
  , lines
  , lines'
  , lineBreakType
  , LineBreaker
  , LineBreakerString(lineBreakerString)
  , LineSplitter
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

lines :: forall (lb :: LineBreakType) c m n a text.
  ( LineSplitter (LineBreaker lb) c m n a
  , text ~ SubState c a
  )
  => Iso c m n a [text]
lines = lineSplitter @(LineBreaker lb)

lines' :: forall c m n a text.
  ( LineSplitter (LineBreaker 'Unix) c m n a
  , LineSplitter (LineBreaker 'Windows) c m n a
  , text ~ SubState c a
  )
  => LineBreakType
  -> Iso c m n a [text]
lines' = \case
  Unix -> lineSplitter @(LineBreaker 'Unix)
  Windows -> lineSplitter @(LineBreaker 'Windows)

class LineSplitter (lb :: Either Char Symbol) c m n a where lineSplitter :: Iso c m n a [SubState c a]
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
  ) => LineSplitter ('Left char) c m n a where
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
  ) => LineSplitter ('Right sym) c m n a where
  lineSplitter = splitOn (symbol @sym)

