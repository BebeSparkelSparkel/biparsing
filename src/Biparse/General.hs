{-# LANGUAGE NoImplicitPrelude #-}
module Biparse.General
  ( Take
  , take
  , take'
  , take''
  , take'''
  , takeNot
  , optionMaybe
  ) where

import Biparse.BiparserT (BiparserT, upon, Iso, uponM, Unit, unit, one, try, SubState, SubElement, StateContext)
import Control.Applicative (Applicative(pure), Alternative((<|>)))
import Control.Monad (Monad(return), when, unless, MonadFail(fail), MonadPlus, guard)
import Data.Eq (Eq((==)))
import Data.Function ((.), ($), const)
import Data.Functor (($>), (<$>), void)
import Data.Maybe (Maybe(Just,Nothing))
import Data.Monoid (Monoid, (<>))
import Data.Sequences (IsSequence)
import Text.Show (Show(show))

type Take c s m n =
  ( Show (SubElement c s)
  , Eq (SubElement c s)
  , IsSequence (SubState c s)
  , MonadFail m
  , MonadFail n
  , StateContext c s
  )

-- | Assumes but disregards the writer context
-- Should not be used with Alternative
take :: forall c s m n u. Take c s m n => SubElement c s -> BiparserT c s m n u ()
take = unit . take'

-- | Discards the match
-- Should not be used with Alternative
take' :: forall c s m n. Take c s m n => SubElement c s -> Unit c s m n
take' x = void $ take'' x `upon` const x

-- | Returns the match
take'' :: forall c s m n. Take c s m n => SubElement c s -> Iso c m n s (SubElement c s)
take'' x = do
  y <- one
  unless (x == y) $ fail $ "Expected a " <> show x <> " but received a " <> show y
  return y

-- | Allows 'SubElement c s'`, 'u', and 'v' to be fixed which works well with Alternative.
take''' :: forall c s m n u v.
  ( Take c s m n
  , Eq u
  , Alternative n
  )
  => SubElement c s
  -> u
  -> v
  -> BiparserT c s m n u v
take''' takeWrite toMatch toReturn = do
  x <- one `uponM` ($> takeWrite) . guard . (== toMatch)
  unless (x == takeWrite) $ fail $ "Expected a " <> show takeWrite <> " but received a " <> show x
  return toReturn

takeNot :: forall c s m n.
  ( Take c s m n
  , MonadPlus m
  )
  => SubElement c s
  -> Iso c m n s (SubElement c s)
takeNot x = try do
  y <- one
  when (x == y) $ fail $ "Should not have found an " <> show y
  return y

optionMaybe :: forall c s m n u v.
  ( Monoid (SubState c s)
  , MonadPlus m
  , Alternative n
  )
  => BiparserT c s m n u v
  -> BiparserT c s m n u (Maybe v)
optionMaybe x = Just <$> try x <|> pure Nothing

