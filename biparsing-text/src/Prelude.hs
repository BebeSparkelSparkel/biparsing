module Prelude
  ( module Biparse.Biparser
  , module Data.Functor.Alt
  , module Data.Convert
  , module Text.Show
  , module Control.Monad.State.Class
  , module Control.Monad.Writer.Class
  , module Data.EqElement
  , module Data.Char
  , module Control.Monad.Fail
  , module Data.Sequences
  , module Data.Eq
  , module Data.Ord
  , module Text.Printf
  , module Control.Monad.Trans.State.Selectable
  , module Data.Function
  , module Data.Monoid
  , module Control.Monad
  , module Data.Maybe
  , module Control.Monad.Except
  , module GHC.Enum
  , module Data.Kind
  , module GHC.TypeLits
  , module Data.String
  , module Data.Bool
  , module Data.Functor
  , module Control.Applicative
  , module Data.Bifunctor
  , module Data.MonoTraversable.Unprefixed
  , module Data.MonoTraversable
  , module Data.Default
  , module Biparse.Utils
  , module Control.Monad.Trans.Writer.Selectable
  , module Lens.Micro.TH
  , module Lens.Micro
  ) where

import Biparse.Biparser (Biparser, pattern Biparser, Iso, Const, IsoClass(iso), SubElement, GetSubState(SubState, getSubState), ReplaceSubState(replaceSubState), InitSuperState(SuperState,fromSubState), SuperArg, ElementContext, SubStateContext, one, comap, upon, uponM, UpdateStateWithElement(updateElementContext), UpdateStateWithSubState(updateSubStateContext), peek, try, ignoreBackward, split)
import Control.Applicative (Applicative((<*>),pure), liftA2)
import Control.Monad (Monad, unless, (=<<))
import Control.Monad.Except (MonadError)
import Control.Monad.Fail (MonadFail(fail))
import Control.Monad.State.Class (MonadState(get,put), modify)
import Control.Monad.Trans.State.Selectable (ContextualStateTransformerPLEASEREMOVESUFFIX, ContextualStateTransformer', state
  , StateTransformer, SelectableStateT)
import Control.Monad.Trans.Writer.Selectable (ContextualWriterTransformer
  , WriterTransformer, SelectableWriterTransformer)
import Control.Monad.Writer.Class (MonadWriter(tell))
import Data.Bifunctor (first, second)
import Data.Bool (Bool(True,False), bool, otherwise, (&&))
import Data.Char (Char)
import Data.Convert (ConvertElement(convertElement), ConvertSequence(convertSequence))
import Data.Eq (Eq((==)))
import Data.EqElement (EqElement)
import Data.Function (($), (&), (.), id, const, flip)
import Data.Functor (Functor(fmap), (<$>), (<$), ($>))
import Data.Functor.Alt (Alt((<!>)))
import Data.Kind (Type, Constraint)
import Data.Maybe (Maybe, maybe)
import Data.MonoTraversable (MonoPointed, MonoFoldable, Element)
import Data.MonoTraversable.Unprefixed (foldr, foldl', null, toList, for_)
import Data.Monoid (Monoid(mempty), (<>))
import Data.Ord (Ord((>),(<)))
import Data.Sequences (IsSequence, fromList, cons, uncons, span, singleton)
import Data.String (IsString(fromString))
import GHC.Enum (Enum(toEnum,fromEnum,pred,succ))
import GHC.TypeLits (Symbol, KnownSymbol, KnownChar)
import Text.Printf (IsChar(toChar,fromChar))
import Text.Show (Show(show))
import Data.Default (Default(def))
import Biparse.Utils ((>>>), headTailAlt)

import Lens.Micro.TH (makeLenses)
import Lens.Micro ((^.), (%~), (.~))
