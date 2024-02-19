{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverloadedStrings #-}
module Biparse.Text.LineBreak
  ( LineBreakType(..)
  , lines
  , linesOn
  , lineBreakType
  , LineBreaker
  , LineBreakerString(..)
  , lineBreakerString'
  , UpdateSuperState
  , LineSplitter(..)
  ) where

import Biparse.Biparser (Iso, SubState, SubElement, SubStateContext, ElementContext)
import Biparse.General (takeDi, takeDi', Take, Take')
import Biparse.List (splitElem, splitOn)
import Biparse.Utils (char)
import Data.Char (Char)
import Biparse.Text (CharElement)
import GHC.TypeLits (ConsSymbol)

data LineBreakType
  = Unix
  | Windows
  deriving (Show, Eq, Ord)

type LineBreaker :: LineBreakType -> Either Char Symbol
type family LineBreaker a where
  LineBreaker 'Unix    = 'Left  '\n'
  LineBreaker 'Windows = 'Right "\r\n"

type LineBreakerToSymbol :: Either Char Symbol -> Symbol
type family LineBreakerToSymbol a where
  LineBreakerToSymbol ('Left c) = ConsSymbol c ""
  LineBreakerToSymbol ('Right sym) = sym

class LineBreakerString (lb :: LineBreakType) where lineBreakerString :: IsString a => a
instance LineBreakerString 'Unix    where lineBreakerString = symbol @(LineBreakerToSymbol (LineBreaker 'Unix))
instance LineBreakerString 'Windows where lineBreakerString = symbol @(LineBreakerToSymbol (LineBreaker 'Windows))

type FromLeft :: Either Char Symbol -> Char
type family FromLeft a where
  FromLeft ('Left a) = a

lineBreakerString' :: IsString a => LineBreakType -> a
lineBreakerString' = \case
  Unix    -> lineBreakerString @'Unix
  Windows -> lineBreakerString @'Windows

lineBreakType :: forall c m n a text se w e.
  ( Take c a m n text se w e
  , Take' c a m n text se w LineBreakType e
  , Alt n
  , CharElement a se
  , IsString text
  ) => Iso c m n a LineBreakType
lineBreakType
  =   takeDi  (char @(FromLeft (LineBreaker 'Unix)))    Unix
  <!> takeDi' (lineBreakerString @'Windows) Windows

lines :: forall (lb :: LineBreakType) c m n a text up.
  ( LineSplitter (LineBreaker lb) up c m n a
  , text ~ SubState a
  , up ~ UpdateSuperState c
  )
  => Iso c m n a [text]
lines = lineSplitter @(LineBreaker lb) @(UpdateSuperState c)

linesOn :: forall c m n a text up.
  ( LineSplitter (LineBreaker 'Unix) up c m n a
  , LineSplitter (LineBreaker 'Windows) up c m n a
  , text ~ SubState a
  , up ~ UpdateSuperState c
  )
  => LineBreakType
  -> Iso c m n a [text]
linesOn = \case
  Unix -> lines @'Unix
  Windows -> lines @'Windows

-- | Used to indicate if 'updateElementContext' and 'updateSubStateContext' should be used. An optimization since 'lines' knows how to update the context without traverseing the consumed substate.
type UpdateSuperState :: Type -> Bool
type family UpdateSuperState a

class LineSplitter (lb :: Either Char Symbol) (up :: Bool) c m n a where
  lineSplitter :: Iso c m n a [SubState a]
instance
  ( MonadState a m
  , MonadWriter w n
  , KnownChar char
  , IsSequence ss
  , CharElement a se
  , ConvertElement c se w
  , ElementContext c a
  , ss ~ SubState a
  , se ~ SubElement a
  ) => LineSplitter ('Left char) 'True c m n a where
  lineSplitter = splitElem @c @a @m @n (char @char)
instance
  ( MonadState a m
  , MonadWriter w n
  , KnownSymbol sym
  , EqElement ss
  , CharElement a se
  , IsString ss
  , Show ss
  , ConvertSequence c ss w
  , ConvertElement c se w
  , SubStateContext c a
  , ElementContext c a
  , ss ~ SubState a
  , se ~ SubElement a
  ) => LineSplitter ('Right sym) 'True c m n a where
  lineSplitter = splitOn (symbol @sym)

