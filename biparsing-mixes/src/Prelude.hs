{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-missing-import-lists #-}
module Prelude
  ( module Data.Function
  , module Data.Bifunctor
  , module Data.Monoid
  , module Text.Show
  , module Data.Sequences
  , module Data.String
  , module System.IO
  , module Biparse.Mixes.SubStates
  , module Control.Monad.StateError

  , Mixes
  , BiparserTemplate
  ) where

import Data.Function ((.), ($), const)
import Data.Bifunctor (first)
import Data.Monoid (Monoid, (<>))
import Text.Show (Show, show)
import Data.Sequences (Index)
import Data.String (String)
import System.IO (FilePath)
import Biparse.Mixes.SubStates
import Control.Monad.StateError (ErrorContext, ErrorInstance(NewtypeInstance,ErrorStateInstance))

import Biparse.AssociatedWriter (AssociatedWriter)
import Biparse.Biparser.StateReaderWriter qualified as SRW
import Control.Applicative (Applicative, pure)
import Data.Char (Char)
import Data.Functor (Functor)
import Control.Monad.Trans.RWS.CPS (RWST, rwsT, runRWST)
import Biparse.Biparser.StateReaderWriter (BackwardC(BackwardT,backwardT,runBackwardT))
import Biparse.Biparser (UpdateStateWithElement, updateElementContext, UpdateStateWithSubState, updateSubStateContext, UpdateStateWithNConsumed, updateStateWithNConsumed, ConvertElement, convertElement, ConvertSequence, convertSequence, SuperState)
import Data.Word (Word8)
import Data.ByteString.Builder qualified as B
import Data.Text.Lazy.Builder qualified as T

-- * Mixes Context

data Mixes c

instance UpdateStateWithElement c s => UpdateStateWithElement (Mixes c) s where updateElementContext = updateElementContext @c @s
instance UpdateStateWithSubState c s => UpdateStateWithSubState (Mixes c) s where updateSubStateContext = updateSubStateContext @c @s
instance UpdateStateWithNConsumed c s => UpdateStateWithNConsumed (Mixes c) s where updateStateWithNConsumed = updateStateWithNConsumed @c @s

instance (Functor n, Monoid w) => BackwardC (Mixes c) n w where
  type BackwardT (Mixes _) = RWST
  backwardT = rwsT
  runBackwardT = runRWST

instance Applicative m => ConvertElement (Mixes c) Char String m where
  convertElement = pure . (: [])
instance Applicative m => ConvertElement (Mixes c) Word8 BuilderByteString m where
  convertElement = pure . B.word8
instance Applicative m => ConvertElement (Mixes c) Char BuilderText m where
  convertElement = pure . T.singleton

instance Applicative m => ConvertSequence (Mixes c) [a] [a] m where
  convertSequence = pure
instance Applicative m => ConvertSequence (Mixes c) StrictByteString BuilderByteString m where
  convertSequence = pure . B.byteString
instance Applicative m => ConvertSequence (Mixes c) LazyByteString BuilderByteString m where
  convertSequence = pure . B.lazyByteString
instance Applicative m => ConvertSequence (Mixes c) StrictText BuilderText m where
  convertSequence = pure . T.fromText
instance Applicative m => ConvertSequence (Mixes c) LazyText BuilderText m where
  convertSequence = pure . T.fromLazyText

-- * Biparser Template

type BiparserTemplate fm bm c ss r ws = SRW.Biparser (Mixes c) (SuperState c ss) fm bm r (AssociatedWriter ss) ws

