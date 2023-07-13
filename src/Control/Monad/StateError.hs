{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}
module Control.Monad.StateError
  ( StateErrorT(..)
  , stateErrorT
  , runSET
  , WrapError(..)
  ) where

import Biparse.Context.IdentityState (IdentityState)
import System.IO (IO)
import GHC.IO.Exception (IOException, userError)
import Control.Monad.Except (catchError)

-- * Allow errors to be combined with state information.

newtype StateErrorT c s m a = StateErrorT {runStateErrorT :: StateT s m a}
  deriving (Functor, Applicative, Alternative, Monad, MonadPlus)

deriving instance Monad m => MonadState s (StateErrorT c s m)
deriving instance MonadFail m => MonadFail (StateErrorT IdentityState s m)

deriving instance {-# OVERLAPS #-}  MonadError IOException (StateErrorT IdentityState s IO)
deriving instance {-# OVERLAPPABLE #-} MonadError e m => MonadError e (StateErrorT IdentityState s m)

stateErrorT :: forall c s m a. (s -> m (a, s)) -> StateErrorT c s m a
stateErrorT = StateErrorT . StateT

runSET :: forall c s m a. StateErrorT c s m a -> s -> m (a, s)
runSET = runStateT . runStateErrorT

-- * Allows including state in the error.

class WrapError c s (m :: Type -> Type) where
  type Error c s m :: Type
  type SubError c s m :: Type
  wrapError :: (SubError c s m, s) -> Error c s m

instance WrapError IdentityState s IO where
  type Error IdentityState s IO = IOException
  type SubError IdentityState s IO = IOException
  wrapError = fst

instance WrapError IdentityState s Maybe where
  type Error IdentityState s Maybe = ()
  type SubError IdentityState s Maybe = ()
  wrapError = const ()

instance WrapError IdentityState s Identity where
  type Error IdentityState s Identity = Void
  type SubError IdentityState s Identity = Void
  wrapError = fst

instance MonadError Void Identity where
   throwError = absurd
   catchError = const
