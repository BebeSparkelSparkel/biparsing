{-# LANGUAGE NoImplicitPrelude #-}
module Biparse.General
  ( Take
  , take
  , takeUnit
  , takeUni
  , takeDi
  , takeTri
  , takeNot
  , takeWhile
  , optionMaybe
  , stripPrefix
  , count
  , countSome
  , FromNatural(..)
  , not
  , isNull
  , memptyBack
  ) where

import Data.Ord ((>))
import Data.Traversable (Traversable)
import Data.Int (Int)
import GHC.Enum (toEnum, fromEnum)
import Numeric.Natural (Natural)
import Control.Monad.Trans.State.Lazy (StateT(StateT), state, get)
import Data.Bool (Bool)
import Data.Bool qualified as Data.Bool
import Biparse.BiparserT (BiparserT, upon, Iso, uponM, Unit, unit, one, try, SubState, SubElement, ElementContext, SubStateContext, split, Const, ConstU, mapBack, mapMs)
import Control.Applicative (Applicative(pure), Alternative(empty,(<|>)))
import Control.Monad (Monad(return), when, unless, MonadFail(fail), MonadPlus, guard)
import Data.Eq (Eq((==)))
import Data.Function ((.), ($), const, flip)
import Data.Functor (Functor(fmap), ($>), (<$>), void)
import Data.Functor.Identity (Identity)
import Data.Maybe (Maybe(Just,Nothing), maybe)
import Data.Monoid (Monoid(mempty), (<>))
import Data.Sequences (IsSequence, span, replicate)
import Data.MonoTraversable.Unprefixed (length, null)
import Data.MonoTraversable (MonoFoldable)
import Data.Sequences qualified as MT
import Text.Show (Show(show))

type Take c s m n =
  ( Show (SubElement c s)
  , Eq (SubElement c s)
  , IsSequence (SubState c s)
  , MonadFail m
  , MonadFail n
  , ElementContext c s
  )

-- | Assumes but disregards the writer context
-- Should not be used with Alternative
take :: forall c s m n u. Take c s m n => SubElement c s -> Const c s m n u
take = unit . takeUnit

-- | Discards the match
-- Should not be used with Alternative
takeUnit :: forall c s m n. Take c s m n => SubElement c s -> Unit c s m n
takeUnit x = void $ takeUni x `upon` const x

-- | Returns the match
takeUni :: forall c s m n se.
  ( Take c s m n
  , se ~ SubElement c s
  )
  => se
  -> Iso c m n s se
takeUni x = do
  y <- one
  unless (x == y) $ expectedFail x y
  return y

takeDi :: forall c s m n u.
  ( Take c s m n
  , Eq u
  , Alternative n
  )
  => SubElement c s
  -> u
  -> Iso c m n s u
takeDi x y = takeTri x y y

-- | Allows 'SubElement c s'`, 'u', and 'v' to be fixed which works well with Alternative.
takeTri :: forall c s m n u v.
  ( Take c s m n
  , Eq u
  , Alternative n
  )
  => SubElement c s
  -> u
  -> v
  -> BiparserT c s m n u v
takeTri takeWrite toMatch toReturn = do
  x <- one `uponM` ($> takeWrite) . guard . (== toMatch)
  unless (takeWrite == x) $ expectedFail takeWrite x
  return toReturn

expectedFail :: (MonadFail m, Show a, Show b) => a -> b -> m ()
expectedFail x y = fail $ "Expected a " <> show x <> " but received a " <> show y

takeNot :: forall c s m n se.
  ( Take c s m n
  , MonadPlus m
  , se ~ SubElement c s
  )
  => se
  -> Iso c m n s se
takeNot x = try do
  y <- one
  when (x == y) $ fail $ "Should not have found an " <> show y
  return y

type TakeWhile c s m n = 
  ( SubStateContext c s
  , IsSequence (SubState c s)
  , Monad m
  , Monad n
  )

takeWhile :: forall c s m n.
  TakeWhile c s m n
  => (SubElement c s -> Bool)
  -> Iso c m n s (SubState c s)
takeWhile = split . state . span

optionMaybe :: forall c s m n u v.
  ( Monoid (SubState c s)
  , MonadPlus m
  , Alternative n
  )
  => BiparserT c s m n u v
  -> BiparserT c s m n u (Maybe v)
optionMaybe x = Just <$> try x <|> pure Nothing

stripPrefix :: forall c s m n ss u.
  ( ss ~ SubState c s
  , IsSequence ss
  , Eq (SubElement c s)
  , SubStateContext c s
  , Show ss
  , MonadFail m
  , Monad n
  )
  => ss
  -> Const c s m n u
stripPrefix pre = unit $ void s `upon` const pre
  where
  s :: Iso c m n s ss
  s = split $ StateT
    $ maybe
      (fail $ "Could not strip prefix: " <> show pre)
      (pure . (pre,))
    . MT.stripPrefix pre

-- | Counts 0 or more elements
count :: forall c s m n se.
  ( FromNatural (MT.Index (SubState c s))
  , Eq se
  , TakeWhile c s m n
  , se ~ SubElement c s
  )
  => se
  -> Iso c m n s Natural
count x = toEnum . length <$> takeWhile (== x) `upon` flip replicate x . fromNatural

-- | Counts 1 or more elements
countSome x = do
  c <- count x
  unless (c > 0) empty
  return c


class FromNatural a where fromNatural :: Natural -> a
instance FromNatural Int where fromNatural = fromEnum

not :: forall c s m n u.
  ( Functor m
  , Functor n
  )
  => BiparserT c s m n u Bool
  -> BiparserT c s m n u Bool
not = fmap Data.Bool.not

-- | Returns true if the substate is empty.
isNull :: forall c s m n u ss.
  ( Monoid ss
  , MonoFoldable ss
  , SubStateContext c s
  , Monad m
  , Monad n
  , ss ~ SubState c s
  )
  => ConstU c s m n u Bool
isNull = null <$> split get `upon` const mempty

-- | Causes backward to write nothing.
memptyBack :: forall c s m n u v.
  ( Monoid (SubState c s)
  , Functor n
  )
  => BiparserT c s m n u v
  -> BiparserT c s m n u v
memptyBack = flip mapBack (const mempty)

