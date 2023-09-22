{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverloadedStrings #-}
module Biparse.Text.LineBreak
  ( LineBreakType(..)
  , lines
  , lineBreak
  --, LineBreakText(..)
  ) where

import Biparse.Biparser (Iso, SubState, UpdateStateWithSubState)
import Biparse.List (splitElem, splitOn)
import Biparse.General (takeDi, takeDi', Take, Take', BreakWhen)
import Biparse.Utils (char)
import Text.Printf (fromChar)

data LineBreakType
  = Unix -- \n
  | Windows -- \r\n
  deriving (Show, Eq, Ord)

--type WhichLineBreak :: Type -> LineBreakType
--type family WhichLineBreak a
--type instance WhichLineBreak (LineColumn lb) = lb

type family LineBreaker (a :: LineBreakType) where
  LineBreaker 'Unix = 'Left '\n'
  LineBreaker 'Windows = 'Right "\r\n"

--class LineBreakText (lb :: LineBreakType) where lineBreakText :: IsString a => a
--instance LineBreakText 'Unix where lineBreakText = "\n"
--instance LineBreakText 'Windows where lineBreakText = "\r\n"

lineBreak :: forall c m n text ss se.
  ( Take c text m n ss se
  , Take' c text m n ss se
  , IsChar se
  , Alternative n
  , IsString ss
  ) => Iso c m n text LineBreakType
lineBreak
  =   takeDi  (fromChar '\n')   Unix
  <|> takeDi' "\r\n" Windows

lines :: forall (lb :: LineBreakType) c m n s text.
  ( LineSplitter (LineBreaker lb) c s m n
  , text ~ SubState c s
  )
  => Iso c m n s [text]
lines = lineSplitter @(LineBreaker lb) -- splitWith $ string $ lineBreakText @lb

class LineSplitter (a :: Either Char Symbol) c s m n where lineSplitter :: Iso c m n s [SubState c s]
instance
  ( KnownChar char
  , IsChar se
  , Alternative n
  , Take c s m n text se
  ) => LineSplitter ('Left char) c s m n where
  lineSplitter = splitElem @c @s @m @n (char @char)
instance
  ( KnownSymbol sym
  , IsString ss
  , BreakWhen c s m n ss
  , UpdateStateWithSubState c s
  , Eq (Element ss)
  , Show ss
  ) => LineSplitter ('Right sym) c s m n where
  lineSplitter = splitOn (symbol @sym)

