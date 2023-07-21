{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DataKinds #-}
module Control.Monad.StateError
  ( StateErrorT(..)
  , ErrorState(..)
  , ErrorInstance(..)
  , ErrorContext
  , stateErrorT
  , runSET
  , ResultMonad(..)
  ) where

import Control.Monad.Except (catchError)
import Control.Monad.ChangeMonad (ChangeMonad(ChangeFunction,changeMonad), ResultMonad(ResultingMonad,resultMonad))
import Biparse.Error.WrapError (WrapError(Error), wrapError)

-- * Allow errors to be combined with state information.

newtype StateErrorT (i :: ErrorInstance) s m a = StateErrorT {runStateErrorT :: StateT s m a}
  deriving (Functor, Applicative, Monad)

-- | Used to determine the instances to use for error handling from the context
data ErrorInstance
  = NewtypeInstance -- | Use the StateT instance
  | ErrorStateInstance -- | Use MonadError (ErrorStateA e s) m

type ErrorContext :: Type -> ErrorInstance
type family ErrorContext c

deriving instance Monad m => MonadState s (StateErrorT i s m)

data ErrorState e s = ErrorState {error :: e, state :: s} deriving (Show, Eq)
instance Bifunctor ErrorState where
  first f (ErrorState e s) = ErrorState (f e) s
  second f (ErrorState e s) = ErrorState e (f s)

deriving instance MonadPlus m => Alternative (StateErrorT 'NewtypeInstance s m)
instance (Monoid e, MonadError (ErrorState e s) m, MonadPlus m) => Alternative (StateErrorT 'ErrorStateInstance s m) where
  empty = throwError mempty
  x <|> y = StateErrorT $ runStateErrorT x <|> runStateErrorT y

deriving instance MonadPlus m => MonadPlus (StateErrorT 'NewtypeInstance s m)
deriving instance (Monoid e, MonadError (ErrorState e s) m, MonadPlus m) => MonadPlus (StateErrorT 'ErrorStateInstance s m)

deriving instance MonadFail m => MonadFail (StateErrorT 'NewtypeInstance s m)
instance MonadError (ErrorState String s) m => MonadFail (StateErrorT 'ErrorStateInstance s m) where
  fail msg = throwError msg

deriving instance MonadError e m => MonadError e (StateErrorT 'NewtypeInstance s m)
instance MonadError (ErrorState e s) m => MonadError e (StateErrorT 'ErrorStateInstance s m) where
  throwError e = stateErrorT \s -> throwError $ ErrorState e s
  catchError x eh = stateErrorT \s -> catchError (r x s) \(ErrorState e s') -> r (eh e) s'
    where r = runStateT . runStateErrorT

stateErrorT :: forall c s m a. (s -> m (a, s)) -> StateErrorT c s m a
stateErrorT = StateErrorT . StateT

runSET :: forall c s m a.
  ( ChangeMonad m (ResultingMonad m)
  , ResultMonad m
  )
  => StateErrorT c s m a
  -> s
  -> ResultingMonad m (a, s)
runSET = (changeMonad (resultMonad @m) .) . runStateT . runStateErrorT

instance
  ( ChangeFunction (Either (ErrorState e s)) (Either (Error e s)) ~ (ErrorState e s -> Error e s)
  , WrapError e s
  ) => ResultMonad (Either (ErrorState e s)) where
  type ResultingMonad (Either (ErrorState e s)) = Either (Error e s)
  resultMonad (ErrorState e s) = wrapError e s

