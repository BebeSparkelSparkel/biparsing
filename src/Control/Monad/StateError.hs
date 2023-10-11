{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DataKinds #-}
module Control.Monad.StateError
  ( StateErrorT(..)
  , M
  , ErrorState(..)
  , ErrorInstance(..)
  , ErrorContext
  , stateErrorT
  , runSET
  , ResultMonad(..)
  ) where

import Biparse.Error.WrapError (WrapError(Error), wrapError)
import Control.Monad.ChangeMonad (ChangeMonad(ChangeFunction,changeMonad'), ResultMonad(ResultingMonad,resultMonad), Lift)
import Control.Monad.Except (catchError)
import Control.Monad.TransformerBaseMonad (TransformerBaseMonad, LiftBaseMonad, liftBaseMonad)

-- * Allow errors to be combined with state information.

newtype StateErrorT (i :: ErrorInstance) s m a = StateErrorT {runStateErrorT :: StateT s m a}
  deriving (Functor, Applicative, Monad, MonadTrans)
type M c a m = StateErrorT (ErrorContext c) a m

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
  fail = throwError

deriving instance MonadError e m => MonadError e (StateErrorT 'NewtypeInstance s m)
instance MonadError (ErrorState e s) m => MonadError e (StateErrorT 'ErrorStateInstance s m) where
  throwError e = stateErrorT $ throwError . ErrorState e
  catchError x eh = stateErrorT \s -> catchError (r x s) \(ErrorState e s') -> r (eh e) s'
    where r = runStateT . runStateErrorT

stateErrorT :: forall c s m a. (s -> m (a, s)) -> StateErrorT c s m a
stateErrorT = StateErrorT . StateT

runSET :: forall is c s m a.
  ( ChangeMonad is m (ResultingMonad m is)
  , ResultMonad m is
  )
  => StateErrorT c s m a
  -> s
  -> ResultingMonad m is (a, s)
runSET = (changeMonad' @is (resultMonad @m @is) .) . runStateT . runStateErrorT

instance
  ( ChangeFunction is (Either (ErrorState e s)) (Either (Error e s)) ~ (ErrorState e s -> Error e s)
  , WrapError e s
  ) => ResultMonad (Either (ErrorState e s)) is where
  type ResultingMonad (Either (ErrorState e s)) is = Either (Error e s)
  resultMonad (ErrorState e s) = wrapError e s

instance Monad m => ChangeMonad Lift m (StateErrorT 'NewtypeInstance s m) where
  type ChangeFunction Lift m (StateErrorT 'NewtypeInstance s m) = ()
  changeMonad' () = lift

type instance TransformerBaseMonad (StateErrorT _ _ m) = m

instance Monad m => LiftBaseMonad (StateErrorT c s m) where liftBaseMonad = lift

