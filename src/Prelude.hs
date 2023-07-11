{-# LANGUAGE NoImplicitPrelude #-}
module Prelude
  ( module Control.Applicative
  , module Control.Monad
  , module Control.Monad.Extra
  , module Control.Monad.State.Class
  , module Control.Monad.Writer
  , module Control.Monad.Writer.Class
  , module Data.Bifunctor
  , module Data.Bool
  , module Data.Char
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
  , module Data.Tuple
  , module GHC.Enum
  , module GHC.Num
  , module Numeric.Natural
  , module Text.Show
  , module Text.Read
  , module Data.Coerce
  , module Control.Monad.State
  , module Control.Monad.Except

  , (|>)
  , (^:^)
  , (<$$>)
  , (<$$)
  , (>>>)
  ) where

import Control.Monad.Except (MonadError(throwError))
import Data.Coerce (Coercible, coerce)
import Control.Applicative (Applicative((<*>),pure), (*>), (<*), liftA2, Alternative(empty,(<|>)))
import Control.Monad (Monad((>>=),return), when, unless, MonadFail(fail), MonadPlus, guard, (=<<), (>=>))
import Control.Monad.Extra (ifM)
import Control.Monad.State.Class (MonadState(state,get,put), modify, gets)
import Control.Monad.Writer (WriterT(WriterT,runWriterT))
import Control.Monad.Writer.Class (MonadWriter(tell,pass))
import Data.Bifunctor (second)
import Data.Bool (Bool(True,False), otherwise, bool)
import Data.Char (Char)
import Data.Either (Either, fromRight)
import Data.Eq (Eq((==)))
import Data.Function (($), (.), (&), id, const, flip)
import Data.Functor (Functor(fmap), (<$), ($>), (<$>), void)
import Data.Functor.Identity (Identity(Identity,runIdentity))
import Data.Int (Int)
import Data.Kind (Type)
import Data.Maybe (Maybe(Just,Nothing), maybe, fromMaybe)
import Data.MonoTraversable (MonoFoldable, headMay, Element, lastMay)
import Data.MonoTraversable.Unprefixed (for_, null, length, toList)
import Data.Monoid (Monoid(mempty))
import Data.Ord (Ord, (>), (>=))
import Data.Semigroup (Semigroup, (<>))
import Data.Sequences (IsSequence, initMay, tailMay, singleton, cons, snoc, span, replicate, Index, fromList, initDef, lengthIndex, initTails)
import Data.String (String, IsString(fromString))
import Data.Tuple (fst, snd)
import GHC.Enum (toEnum, fromEnum)
import GHC.Num (Num, (+), (-))
import Numeric.Natural (Natural)
import Text.Show (Show(show))
import Text.Read (Read)
import Control.Monad.State (StateT(StateT,runStateT), execState)

(|>) :: Alternative f => f a -> a -> f a
x |> y = x <|> pure y

infixr 5 ^:^
(^:^) :: Applicative f => f a -> f [a] -> f [a]
(^:^) = liftA2 (:)

infixl 4 <$$>
(<$$>) :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
(<$$>) = fmap . fmap

infixl 4 <$$
(<$$) :: (Functor f, Functor g) => b -> f (g a) -> f (g b)
(<$$) = fmap . (<$)

infixr 9 >>>
(>>>) :: (a -> b) -> (b -> c) -> a -> c
(>>>) = flip (.)

