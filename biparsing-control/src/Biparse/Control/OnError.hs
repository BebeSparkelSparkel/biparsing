{-# LANGUAGE NoImplicitPrelude #-}
module Biparse.Control.OnError (OnError(..)) where

import Biparse.Core.Aliases (IdentityT(IdentityT), ReaderT(ReaderT), CPSWriterT, pattern CPSWriterT, LazyWriterT, pattern LazyWriterT, StrictWriterT, pattern StrictWriterT, LazyStateT, pattern LazyStateT, StrictStateT, pattern StrictStateT, CPSRWST, pattern CPSRWST, LazyRWST, pattern LazyRWST, StrictRWST, pattern StrictRWST)
import Control.Monad (Monad)
import Control.Monad.Catch qualified
import Data.Either (Either)
import Data.Function (const, ($))
import Data.Maybe (Maybe)
import Data.Monoid (Monoid)
import GHC.IO (IO)

class Monad m => OnError m where onError :: m a -> m b -> m a
deriving instance OnError m => OnError (IdentityT m)
instance OnError m => OnError (ReaderT r m) where
  onError (ReaderT x) (ReaderT y) = ReaderT \r -> onError (x r) (y r)
instance (OnError m, Monoid w) => OnError (CPSWriterT w m) where
  onError (CPSWriterT x) (CPSWriterT y) = CPSWriterT $ onError x y
instance (OnError m, Monoid w) => OnError (LazyWriterT w m) where
  onError (LazyWriterT x) (LazyWriterT y) = LazyWriterT $ onError x y
instance (OnError m, Monoid w) => OnError (StrictWriterT w m) where
  onError (StrictWriterT x) (StrictWriterT y) = StrictWriterT $ onError x y
instance OnError m => OnError (LazyStateT s m) where
  onError (LazyStateT x) (LazyStateT y) = LazyStateT \s -> onError (x s) (y s)
instance OnError m => OnError (StrictStateT s m) where
  onError (StrictStateT x) (StrictStateT y) = StrictStateT \s -> onError (x s) (y s)
instance (OnError m, Monoid w) => OnError (CPSRWST r w s m) where
  onError (CPSRWST x) (CPSRWST y) = CPSRWST \r s -> onError (x r s) (y r s)
instance (OnError m, Monoid w) => OnError (LazyRWST r w s m) where
  onError (LazyRWST x) (LazyRWST y) = LazyRWST \r s -> onError (x r s) (y r s)
instance (OnError m, Monoid w) => OnError (StrictRWST r w s m) where
  onError (StrictRWST x) (StrictRWST y) = StrictRWST \r s -> onError (x r s) (y r s)
instance OnError IO where onError = Control.Monad.Catch.onError
instance OnError Maybe where onError = const
instance OnError (Either e) where onError = const
