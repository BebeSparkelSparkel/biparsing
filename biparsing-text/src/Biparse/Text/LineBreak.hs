{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverloadedStrings #-}
module Biparse.Text.LineBreak
  ( LineBreakType(..)
  , lines
  , lines'
  , linesOn
  , linesOn'
  , lineBreakType
  , LineBreaker
  , LineBreakerString(..)
  , lineBreakerString'
  , UpdateSuperState
  , LineSplitter(..)
  ) where

import Biparse.List (splitElem, splitOn)
import Biparse.General (takeDi, takeDi', Take, Take')
import Biparse.Utils (char, symbol)
import Biparse.Text (CharElement)
import GHC.TypeLits (ConsSymbol)
import Data.Either (Either(Left,Right))

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
  ( LineSplitter (LineBreaker lb) up c m n a [text]
  , up ~ UpdateSuperState c
  ) => Iso c m n a [text]
lines = lines' @lb

lines' :: forall (lb :: LineBreakType) c m n a up seq.
  ( LineSplitter (LineBreaker lb) up c m n a seq
  , up ~ UpdateSuperState c
  ) => Iso c m n a seq
lines' = lineSplitter @(LineBreaker lb) @(UpdateSuperState c)

linesOn :: forall c m n a text up.
  ( LineSplitter (LineBreaker 'Unix) up c m n a [text]
  , LineSplitter (LineBreaker 'Windows) up c m n a [text]
  , up ~ UpdateSuperState c
  ) => LineBreakType
  -> Iso c m n a [text]
linesOn = linesOn'

linesOn' :: forall c m n a up seq.
  ( LineSplitter (LineBreaker 'Unix) up c m n a seq
  , LineSplitter (LineBreaker 'Windows) up c m n a seq
  , up ~ UpdateSuperState c
  ) => LineBreakType
  -> Iso c m n a seq
linesOn' = \case
  Unix    -> lines' @'Unix
  Windows -> lines' @'Windows

-- | Used to indicate if 'updateElementContext' and 'updateSubStateContext' should be used. An optimization since 'lines' knows how to update the context without traverseing the consumed substate.
type UpdateSuperState :: Type -> Bool
type family UpdateSuperState a

class LineSplitter (lb :: Either Char Symbol) (up :: Bool) c m n a seq where
  lineSplitter :: Iso c m n a seq
instance
  ( MonadState a m
  , MonadWriter w n
  , KnownChar char
  , IsSequence ss
  , CharElement a se
  , ContextualStateTransformer' c a Maybe m'
  , MonadError e m'
  , MonadFail m'
  , Alt m'
  , ContextualWriterTransformer c w Maybe n'
  , MonadFail n'
  , Alt n'
  , ConvertElement c se w n'
  , ConvertSequence c [ss] seq (Biparser c a m n seq)
  , ConvertSequence c seq [ss] n
  , ElementContext c a
  , ss ~ SubState a
  , se ~ SubElement a
  ) => LineSplitter ('Left char) 'True c m n a seq where
  lineSplitter = convertSequence @c =<< splitElem @c @a @m @n (char @char) `uponM` convertSequence @c
instance
  ( MonadState a m
  , MonadWriter w n
  , KnownSymbol sym
  , EqElement ss
  , CharElement a se
  , IsString ss
  , Show ss
  , ContextualStateTransformer' c a Maybe m'
  , MonadError e m'
  , MonadFail m'
  , Alt m'
  , MonadState ss (StateTransformer c ss m')
  , ContextualWriterTransformer c w Maybe n'
  , ConvertSequence c ss w n'
  , MonadFail n'
  , Alt n'
  , ConvertSequence c [ss] seq (Biparser c a m n seq)
  , ConvertSequence c seq [ss] n
  , SubStateContext c a
  , ElementContext c a
  , ss ~ SubState a
  , se ~ SubElement a
  ) => LineSplitter ('Right sym) 'True c m n a seq where
  lineSplitter = convertSequence @c =<< splitOn (symbol @sym) `uponM` convertSequence @c

