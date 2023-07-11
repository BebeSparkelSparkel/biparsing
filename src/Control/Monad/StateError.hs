{-# LANGUAGE AllowAmbiguousTypes #-}
module Control.Monad.StateError
  ( StateErrorT(..)
  , stateErrorT
  , runSET
  , WrapError(..)
  ) where

import Biparse.Biparser (IdentityStateContext)

-- * Allow errors to be combined with state information.

newtype StateErrorT c s m a = StateErrorT {runStateErrorT :: StateT s m a}
  deriving (Functor, Applicative, Alternative, Monad, MonadPlus)

deriving instance Monad m => MonadState s (StateErrorT c s m)
deriving instance MonadFail m => MonadFail (StateErrorT IdentityStateContext s m)

stateErrorT :: forall c s m a. (s -> m (a, s)) -> StateErrorT c s m a
stateErrorT = StateErrorT . StateT

runSET :: forall c s m a. StateErrorT c s m a -> s -> m (a, s)
runSET = runStateT . runStateErrorT

-- * Allows including state in the error.

class WrapError c s where
  type Error c s :: Type
  type SubError c s :: Type
  wrapError :: (SubError c s, s) -> Error c s

