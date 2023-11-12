{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# OPTIONS_GHC -Wno-deprecations -Wno-orphans #-}
module Control.Monad.StateError
  ( StateErrorT(StateErrorT)
  , runStateErrorT
  , M
  , ErrorState(..)
  , ErrorInstance(..)
  , ErrorContext
  , runSET
  , wrapErrorWithState
  , WrapErrorWithState(..)
  , MonadProgenitor
  ) where

import Control.Monad.ChangeMonad (ChangeMonad(ChangeFunction,changeMonad'), ResultMonad(ResultingMonad,resultMonad), Lift)
import Control.Monad.Unrecoverable (MonadUnrecoverable, throwUnrecoverable, UnrecoverableError)
import Control.Monad.TransformerBaseMonad (TransformerBaseMonad, LiftBaseMonad, liftBaseMonad)

import Control.Exception (IOException)
import GHC.Err (undefined)
import Control.Monad.Trans.Error (Error, noMsg)

-- * Allow errors to be combined with state information.

newtype StateErrorT (i :: ErrorInstance) s m a = StateErrorT' (StateT s m a)
  deriving (Functor, Applicative, Monad, MonadTrans)
type M c s m = StateErrorT (ErrorContext c) s m

{-# COMPLETE StateErrorT #-}
pattern StateErrorT :: (s -> m (a, s)) -> StateErrorT i s m a
pattern StateErrorT z = StateErrorT' (StateT z)

runStateErrorT :: StateErrorT i s m a -> s -> m (a, s)
runStateErrorT (StateErrorT x) = x

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
instance WrapErrorWithState (ErrorState e s) s (ErrorState e s) where
  type StateForError (ErrorState e s) s (ErrorState e s) = s
  wrapErrorWithState' = ErrorState . error
  stateForError = id
instance Error (ErrorState e p) where
  noMsg = undefined

deriving instance MonadPlus m => Alternative (StateErrorT 'NewtypeInstance s m)
instance (Monoid e, MonadError (ErrorState e s) m, MonadPlus m) => Alternative (StateErrorT 'ErrorStateInstance s m) where
  empty = throwError mempty
  StateErrorT' x <|> StateErrorT' y = StateErrorT' $ x <|> y

deriving instance MonadPlus m => MonadPlus (StateErrorT 'NewtypeInstance s m)
deriving instance (Monoid e, MonadError (ErrorState e s) m, MonadPlus m) => MonadPlus (StateErrorT 'ErrorStateInstance s m)

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
  type ChangeFunction Lift m (StateErrorT 'NewtypeInstance s m') = ()
  changeMonad' () = lift . changeMonad' @() @m @m' ()
--instance Monad m => ChangeMonad Lift m (StateErrorT 'NewtypeInstance s m) where
--  type ChangeFunction Lift m (StateErrorT 'NewtypeInstance s m) = ()
--  changeMonad' () = lift

type instance TransformerBaseMonad (StateErrorT _ _ m) = m

instance Monad m => LiftBaseMonad (StateErrorT c s m) where liftBaseMonad = lift

--type DevType e s = StateErrorT 'ErrorStateInstance s (UnrecoverableT (ErrorState e s) (Either (ErrorState e s)))
--instance MonadUnrecoverable (DevType e s) where
--  type UnrecoverableError (DevType e s) = e
--  throwUnrecoverable e = StateErrorT \s -> UnrecoverableT $ Left $ ErrorState e s

type SubError :: Type -> Type
type family SubError e where SubError (ErrorState e _) = e
instance
  ( UnrecoverableError m ~ ErrorState (SubError (UnrecoverableError m)) s
  , MonadUnrecoverable m
  ) => MonadUnrecoverable (StateErrorT i s m) where
  type UnrecoverableError (StateErrorT i s m) = SubError (UnrecoverableError m)
  throwUnrecoverable e = StateErrorT \s -> throwUnrecoverable $ ErrorState e s


-- * Wrapping an error with state information.

wrapErrorWithState :: forall e s er. WrapErrorWithState e s er => e -> s -> er
wrapErrorWithState e s = wrapErrorWithState' @e @s e $ stateForError @e @_ @er s

type WrapErrorWithState :: Type -> Type -> Type -> Constraint
class WrapErrorWithState e s er where
  type StateForError e s er :: Type
  wrapErrorWithState' :: e -> StateForError e s er -> er
  stateForError :: s -> StateForError e s er

instance WrapErrorWithState IOException s IOException where
  type StateForError IOException s IOException = ()
  wrapErrorWithState' = const
  stateForError = const ()

instance WrapErrorWithState Void s Void where
  type StateForError Void s Void = ()
  wrapErrorWithState' = absurd
  stateForError = const ()

instance WrapErrorWithState () s () where
  type StateForError () s () = ()
  wrapErrorWithState' = const
  stateForError = const ()

instance MonadError Void Identity where
  throwError = absurd
  catchError = const

-- * Monad Progenitor for embedding error types

type MonadProgenitor :: k -> Type -> (Type -> Type)
type family MonadProgenitor progenitor state

type instance MonadProgenitor Either s = Either (ErrorState String s)

