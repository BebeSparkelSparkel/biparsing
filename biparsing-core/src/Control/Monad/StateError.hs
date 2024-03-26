{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-deprecations -Wno-orphans #-}
module Control.Monad.StateError
  ( StateErrorT(StateErrorT)
  , runStateErrorT
  , M
  , ErrorState(..)
  , error
  , errorState
  , ErrorInstance(..)
  , ErrorContext
  , runSET
  ) where

import Control.Monad.ChangeMonad (ChangeMonad, ChangeFunction, changeMonad', ResultMonad(ResultingMonad,resultMonad), Lift)
import Control.Monad.Unrecoverable (MonadUnrecoverable, throwUnrecoverable, UnrecoverableError)
import Control.Monad.TransformerBaseMonad (TransformerBaseMonad, LiftBaseMonad, liftBaseMonad)
import Control.Monad.MonadProgenitor (MonadProgenitor)
import Lens.Micro.TH (makeLenses)
import Control.Monad.Reader.Class (MonadReader)
import System.IO (IO)

-- * Allow errors to be combined with state information.

newtype StateErrorT (i :: ErrorInstance) s m a = StateErrorT' (StateT s m a)
  deriving (Functor, Applicative, Monad, MonadTrans)

type M s m = StateErrorT (ErrorContext m) s m

{-# COMPLETE StateErrorT #-}
pattern StateErrorT :: (s -> m (a, s)) -> StateErrorT i s m a
pattern StateErrorT z = StateErrorT' (StateT z)

runStateErrorT :: forall i s m a. StateErrorT i s m a -> s -> m (a, s)
runStateErrorT (StateErrorT x) = x

-- | Used to determine the instances to use for error handling from the context
data ErrorInstance
  = NewtypeInstance -- | Use the StateT instance
  | ErrorStateInstance -- | Use MonadError (ErrorState e s) m

type ErrorContext :: (Type -> Type) -> ErrorInstance
type family ErrorContext m
type instance ErrorContext (Either _) = 'ErrorStateInstance
type instance ErrorContext EitherString = 'NewtypeInstance
type instance ErrorContext IO = 'NewtypeInstance
type instance ErrorContext Maybe = 'NewtypeInstance

deriving instance Monad m => MonadState s (StateErrorT i s m)
deriving instance MonadReader r m => MonadReader r (StateErrorT i s m)

data ErrorState e s = ErrorState {_error :: e, _errorState :: s} deriving (Show, Eq)
$(makeLenses ''ErrorState)
instance Bifunctor ErrorState where
  first f (ErrorState e s) = ErrorState (f e) s
  second f (ErrorState e s) = ErrorState e (f s)

instance Alt m => Alt (StateErrorT i s m) where
  StateErrorT' x <!> StateErrorT' y = StateErrorT' $ x <!> y

deriving instance MonadFail m => MonadFail (StateErrorT 'NewtypeInstance s m)
instance MonadError (ErrorState String s) m => MonadFail (StateErrorT 'ErrorStateInstance s m) where
  fail = throwError

deriving instance MonadError e m => MonadError e (StateErrorT 'NewtypeInstance s m)
instance MonadError (ErrorState e s) m => MonadError e (StateErrorT 'ErrorStateInstance s m) where
  throwError e = StateErrorT $ throwError . ErrorState e
  catchError x eh = StateErrorT \s -> catchError (r x s) \(ErrorState e s') -> r (eh e) s'
    where r = runStateErrorT

runSET :: forall is c s m a.
  ( ChangeMonad is m (ResultingMonad m is)
  , ResultMonad m is
  )
  => StateErrorT c s m a
  -> s
  -> ResultingMonad m is (a, s)
runSET = (changeMonad' @is (resultMonad @m @is) .) . runStateErrorT

instance ResultMonad (Either (ErrorState e (Identity s))) () where
  type ResultingMonad (Either (ErrorState e (Identity s))) () = Either (ErrorState e (Identity s))
  resultMonad = ()

instance (ChangeMonad () m m', ChangeFunction () m m' ~ (), Monad m') => ChangeMonad Lift m (StateErrorT 'NewtypeInstance s m') where
  changeMonad' () = lift . changeMonad' @() @m @m' ()
type instance ChangeFunction Lift _ (StateErrorT 'NewtypeInstance _ _) = ()

type instance TransformerBaseMonad (StateErrorT _ _ m) = m

instance Monad m => LiftBaseMonad (StateErrorT c s m) where liftBaseMonad = lift

type SubError :: Type -> Type
type family SubError e where SubError (ErrorState e _) = e
instance
  ( UnrecoverableError m ~ ErrorState (SubError (UnrecoverableError m)) s
  , MonadUnrecoverable m
  ) => MonadUnrecoverable (StateErrorT i s m) where
  type UnrecoverableError (StateErrorT i s m) = SubError (UnrecoverableError m)
  throwUnrecoverable e = StateErrorT \s -> throwUnrecoverable $ ErrorState e s

type instance MonadProgenitor Either s = Either (ErrorState String s)
type instance MonadProgenitor '(StateErrorT,Either) s = StateErrorT 'ErrorStateInstance s (MonadProgenitor Either s)

