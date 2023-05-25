{-# LANGUAGE NoImplicitPrelude #-}
module Biparse.General
  ( take
  , take'
  , take''
  , take'''
  , takeWhile
  , optionMaybe
  ) where

import Biparse.BiparserT (BiparserT(forward,backward), upon, Iso, uponM, Unit, unit, emptyForward, comapM, one, try)
import Control.Applicative (Applicative(pure), (*>), Alternative((<|>),empty), liftA2)
import Control.Monad (Monad(return, (>>=)), unless, MonadFail(fail), MonadPlus, guard)
import Control.Monad.Trans.State.Lazy (get, put)
import Control.Monad.Trans.Writer.Lazy (tell)
import Data.Bool (Bool)
import Data.Eq (Eq((==)))
import Data.Function ((.), flip, ($), const)
import Data.Functor (($>), (<$>), void)
import Data.Maybe (Maybe(Just,Nothing), maybe)
import Data.MonoTraversable (Element, headMay)
import Data.Monoid (Monoid, (<>), mempty)
import Data.Sequences (IsSequence, tailMay, singleton)
import Text.Show (Show(show))

type Take sw m n =
  ( Show (Element sw)
  , Eq (Element sw)
  , IsSequence sw
  , MonadFail m
  , MonadFail n
  )

-- | Assumes but disregards the writer context
-- Should not be used with Alternative
take :: forall sw m n u. Take sw m n => Element sw -> BiparserT sw m n u ()
take = unit . take'

-- | Discards the match
-- Should not be used with Alternative
take' :: forall sw m n. Take sw m n => Element sw -> Unit sw m n
take' x = void $ take'' x `upon` const x

-- | Returns the match
take'' :: forall sw m n. Take sw m n => Element sw -> Iso m n sw (Element sw)
take'' x = do
  y <- one
  unless (x == y) $ fail $ "Expected a " <> show x <> " but received a " <> show y
  return y

-- | Allows 'Element sw'`, 'u', and 'v' to be fixed which works well with Alternative.
take''' :: forall sw m n u v.
  ( Take sw m n
  , Eq u
  , Alternative n
  )
  => Element sw
  -> u
  -> v
  -> BiparserT sw m n u v
take''' takeWrite toMatch toReturn = do
  x <- one `uponM` ($> takeWrite) . guard . (== toMatch)
  unless (x == takeWrite) $ fail $ "Expected a " <> show takeWrite <> " but received a " <> show x
  return toReturn

takeWhile ::
   ( IsSequence sw
   , MonadFail m
   , MonadPlus m
   , Monad n
   , Alternative n
   )
  => (Element sw -> Bool)
  -> Iso m n sw [Element sw]
takeWhile f =
  try do
    x <- one `uponM` headAlt
    unless (f x) emptyForward
    (x :) <$> takeWhile f `uponM` tailAlt
  <|> return mempty

optionMaybe :: forall sw m n u v.
  ( Monoid sw
  , MonadPlus m
  , Alternative n
  )
  => BiparserT sw m n u v
  -> BiparserT sw m n u (Maybe v)
optionMaybe x = Just <$> try x <|> pure Nothing

headAlt :: Alternative m => [a] -> m a
headAlt = maybe empty pure . headMay

tailAlt :: Alternative m => [a] -> m [a]
tailAlt = maybe empty pure . tailMay

