{-# LANGUAGE UndecidableInstances #-}
module Control.Monad.StateError
  ( StateErrorT(..)
  , ErrorState(..)
  , stateErrorT
  , runSET
  , ResultMonad(..)
  ) where

import Control.Monad.Except (catchError)
import Control.Monad.ChangeMonad (ChangeMonad(ChangeFunction,changeMonad), ResultMonad(ResultingMonad,resultMonad))
import Biparse.Error.WrapError (WrapError(Error,wrapError))
import Control.Monad.Trans.Error qualified as E
import Control.Exception (IOException)

import System.IO (IO)

-- * Allow errors to be combined with state information.

newtype StateErrorT s m a = StateErrorT {runStateErrorT :: StateT s m a}
  deriving (Functor, Applicative, Monad)

deriving instance Monad m => MonadState s (StateErrorT s m)

data ErrorState e s = ErrorState {error :: e, state :: s} deriving (Show, Eq)

deriving instance {-# OVERLAPS #-} Alternative (StateErrorT s Maybe)
deriving instance {-# OVERLAPS #-} Alternative (StateErrorT s IO)
instance {-# OVERLAPPABLE #-} (Monoid e, MonadError (ErrorState e s) m, MonadPlus m) => Alternative (StateErrorT s m) where
  empty = throwError mempty
  x <|> y = StateErrorT $ runStateErrorT x <|> runStateErrorT y

deriving instance {-# OVERLAPS #-} MonadPlus (StateErrorT s Maybe)
deriving instance {-# OVERLAPS #-} MonadPlus (StateErrorT s IO)
deriving instance {-# OVERLAPPABLE #-} (Monoid e, MonadError (ErrorState e s) m, MonadPlus m) => MonadPlus (StateErrorT s m)

deriving instance {-# OVERLAPS #-} MonadFail (StateErrorT s Maybe)
deriving instance {-# OVERLAPS #-} MonadFail (StateErrorT s IO)
instance {-# OVERLAPPABLE #-} MonadError (ErrorState String s) m => MonadFail (StateErrorT s m) where
  fail msg = throwError msg

deriving instance {-# OVERLAPS #-} MonadError () (StateErrorT s Maybe)
deriving instance {-# OVERLAPS #-} MonadError IOException (StateErrorT s IO)
instance {-# OVERLAPPABLE #-} MonadError (ErrorState e s) m => MonadError e (StateErrorT s m) where
  throwError e = stateErrorT \s -> throwError $ ErrorState e s
  catchError x eh = stateErrorT \s -> catchError (r x s) \(ErrorState e s') -> r (eh e) s'
    where r = runStateT . runStateErrorT

stateErrorT :: forall s m a. (s -> m (a, s)) -> StateErrorT s m a
stateErrorT = StateErrorT . StateT

runSET :: forall s m a.
  ( ChangeMonad m (ResultingMonad m)
  , ResultMonad m
  )
  => StateErrorT s m a
  -> s
  -> ResultingMonad m (a, s)
runSET = (changeMonad (resultMonad @m) .) . runStateT . runStateErrorT

instance
  ( ChangeFunction (Either (ErrorState e s)) (Either (Error e s)) ~ (ErrorState e s -> Error e s)
  , WrapError e s
  ) => ResultMonad (Either (ErrorState e s)) where
  type ResultingMonad (Either (ErrorState e s)) = Either (Error e s)
  resultMonad (ErrorState e s) = wrapError e s

