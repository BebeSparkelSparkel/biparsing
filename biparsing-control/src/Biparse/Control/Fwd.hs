{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ImpredicativeTypes #-}
module Biparse.Control.Fwd (Fwd(..)) where

import Control.Monad.State.Class (MonadState(get,put))
import Debug.Trace (trace)

-- | 'u' is thrown away
newtype Fwd m u a = Fwd {runFwd :: m a}
  deriving (Show, Eq, Functor, Applicative, Monad, MonadFail, Peek, Try)

deriving instance MonadState s m => MonadState s (Fwd m u)
deriving instance MonadError e m => MonadError e (Fwd m u)

instance Alt m => Alt (Fwd m u) where
  Fwd x <!> Fwd y = Fwd $ x <!> y

instance Functor m => Profunctor (Fwd m) where
  dimap _ g (Fwd x) = Fwd $ g <$> x

instance Colift (Fwd m) m where
  colift = const coerce

type instance Item' (Fwd m) = Item' m
instance OneFwd a m => One a (Fwd m) where one = Fwd oneFwd

