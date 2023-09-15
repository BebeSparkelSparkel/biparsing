{-# LANGUAGE NoImplicitPrelude #-}
module Prelude
  ( module Biparse.Utils
  , module Control.Applicative
  , module Control.Monad
  , module Control.Monad.EitherString
  , module Control.Monad.Except
  , module Control.Monad.Extra
  , module Control.Monad.State
  , module Control.Monad.State.Class
  , module Control.Monad.Writer
  , module Control.Monad.Writer.Class
  , module Control.Monad.Trans.Class
  , module Data.Bifunctor
  , module Data.Bool
  , module Data.Char
  , module Data.Coerce
  , module Data.Either
  , module Data.Eq
  , module Data.Function
  , module Data.Functor
  , module Data.Functor.Identity
  , module Data.Int
  , module Data.Kind
  , module Data.Maybe
  , module Data.MonoTraversable
  , module Data.MonoTraversable.Unprefixed
  , module Data.Monoid
  , module Data.Ord
  , module Data.Semigroup
  , module Data.Sequences
  , module Data.String
  , module Data.Traversable
  , module Data.Tuple
  , module Data.Void
  , module GHC.Enum
  , module GHC.Num
  , module Numeric.Natural
  , module Text.Read
  , module Text.Show
  ) where

import Biparse.Utils (headAlt, tailAlt, initAlt, headTailAlt, lastAlt, (^:^), (<$$>), ConvertIntegral(convertIntegral))
import Control.Applicative (Applicative((<*>),pure), (*>), (<*), liftA2, Alternative(empty,(<|>)))
import Control.Monad (Monad((>>=),return), when, unless, MonadFail(fail), MonadPlus, guard, (=<<), (>=>), (<=<))
import Control.Monad.EitherString (EitherString)
import Control.Monad.Except (MonadError(throwError))
import Control.Monad.Extra (ifM)
import Control.Monad.Trans.Class (MonadTrans, lift)
import Control.Monad.State (StateT(StateT,runStateT), execState, execStateT)
import Control.Monad.State.Class (MonadState(state,get,put), modify, gets)
import Control.Monad.Writer (WriterT(WriterT,runWriterT))
import Control.Monad.Writer.Class (MonadWriter(tell,pass))
import Data.Bifunctor (Bifunctor, first, second)
import Data.Bool (Bool(True,False), otherwise, bool, (&&))
import Data.Char (Char)
import Data.Coerce (Coercible, coerce)
import Data.Either (Either(Left), fromRight, either)
import Data.Eq (Eq((==)))
import Data.Function (($), (.), (&), id, const, flip)
import Data.Functor (Functor(fmap), (<$), ($>), (<$>), (<&>), void)
import Data.Functor.Identity (Identity(Identity,runIdentity))
import Data.Int (Int)
import Data.Kind (Type, Constraint)
import Data.Maybe (Maybe(Just,Nothing), maybe, fromMaybe)
import Data.MonoTraversable (MonoFoldable, headMay, Element, lastMay)
import Data.MonoTraversable.Unprefixed (for_, null, length, toList)
import Data.Monoid (Monoid(mempty))
import Data.Ord (Ord, (>), (>=))
import Data.Semigroup (Semigroup, (<>))
import Data.Sequences (IsSequence, SemiSequence, singleton, initTails, Index, span, lengthIndex, replicate, cons, fromList, snoc)
import Data.String (String, IsString(fromString))
import Data.Traversable (traverse, for)
import Data.Tuple (fst, snd, uncurry)
import Data.Void (Void, absurd)
import GHC.Enum (toEnum, fromEnum)
import GHC.Num (Num, (+), (-))
import Numeric.Natural (Natural)
import Text.Read (Read)
import Text.Show (Show(show))

