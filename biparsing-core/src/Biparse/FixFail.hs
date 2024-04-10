{-# LANGUAGE NoImplicitPrelude #-}
module Biparse.FixFail {-# DEPRECATED "Should be put in a specifically pure backwards package" #-}
  ( FixFail(..)
  , FixFailM(..)
  ) where

import Control.Applicative (Applicative(pure))
import Control.Monad.State (StateT(StateT,runStateT))
import Control.Monad.Writer (WriterT(WriterT,runWriterT))
import Data.Either (Either, fromRight)
import Data.Function (($))
import Data.Functor.Identity (Identity(runIdentity))
import Data.Maybe (Maybe, fromMaybe)
import Data.Monoid (Monoid(mempty))

class FixFail m where fixFail :: a -> m a -> a

class FixFailM m where fixFailM :: a -> m a -> m a

instance FixFail Identity where fixFail _ = runIdentity
instance FixFail Maybe where fixFail = fromMaybe
instance FixFail (Either a) where fixFail = fromRight

instance (FixFail m, Applicative m) => FixFailM (StateT s m) where
  fixFailM x y = StateT $ \s -> pure $ (x,s) `fixFail` runStateT y s
instance (Monoid w, FixFail m, Applicative m) => FixFailM (WriterT w m) where
  fixFailM x y = WriterT $ pure $ (x,mempty) `fixFail` runWriterT y

