{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverloadedStrings #-}
module Biparse.Text.LineBreak
  ( LineBreakType(..)
  , lines
  , lineBreakType
  , LineBreaker
  , LineSplitter
  ) where

import Biparse.Biparser (Iso, SubState, UpdateStateWithSubState)
import Biparse.List (splitElem, splitOn)
import Biparse.General (takeDi, takeDi', Take, Take', BreakWhen)
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

class LineSplitter (lb :: Either Char Symbol) c m n a where lineSplitter :: Iso c m n a [SubState c a]
instance
  ( KnownChar char
  , IsChar se
  , Alternative n
  , Take c a m n text se
  ) => LineSplitter ('Left char) c m n a where
  lineSplitter = splitElem @c @a @m @n (char @char)
instance
  ( KnownSymbol sym
  , IsString ss
  , BreakWhen c a m n ss
  , UpdateStateWithSubState c a
  , Eq (Element ss)
  , Show ss
  ) => LineSplitter ('Right sym) c m n a where
  lineSplitter = splitOn (symbol @sym)

