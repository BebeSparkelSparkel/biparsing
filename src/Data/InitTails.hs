{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE UndecidableInstances #-}
module Data.InitTails
  ( InitTails(..)
  ) where

import Data.List (zip)
import Data.Maybe (maybe)
import Data.Semigroup ((<>))
import Data.Function ((.), ($))
import Data.Functor (fmap)
import Data.Tuple (fst, snd)
import Data.Sequences (IsSequence, initMay, tailMay, singleton)
import Data.Monoid (mempty)
import Data.Coerce (coerce)

import Data.List qualified
import Data.Text qualified

class InitTails a where
  initTails :: a -> [(a,a)]
  initTails x = zip (inits x) (tails x)
  inits :: a -> [a]
  inits = fmap fst . initTails
  tails :: a -> [a]
  tails = fmap snd . initTails

instance InitTails [a] where
  inits = Data.List.inits
  tails = Data.List.tails

instance InitTails Data.Text.Text where
  inits = Data.Text.inits
  tails = Data.Text.tails

newtype DefaultInstance a = DefaultInstance {unDefaultInstance :: a}
-- | DEV NOTE: A terrible implementation
instance {-# OVERLAPPABLE #-} IsSequence a => InitTails (DefaultInstance a) where
  inits (DefaultInstance start) = coerce $ is start <> [start]
    where
    is :: a -> [a]
    is = maybe mempty (\y -> is y <> singleton y) . initMay
  tails = coerce . ts . unDefaultInstance
    where
    ts :: a -> [a]
    ts = maybe mempty (\x -> x : ts x) . tailMay 

