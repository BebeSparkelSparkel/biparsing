{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DataKinds #-}
module Control.Monad.StateError
  ( StateErrorT(StateErrorT)
  , runStateErrorT
  , M
  , ErrorState(..)
  , ErrorInstance(..)
  , ErrorContext
  , runSET
  , ResultMonad(..)
  ) where

import Biparse.Error.WrapError (WrapError(Error, StateForError, wrapError', stateForError), wrapError)
import Control.Monad.ChangeMonad (ChangeMonad(ChangeFunction,changeMonad'), ResultMonad(ResultingMonad,resultMonad), Lift)
import Control.Monad.Unrecoverable (MonadUnrecoverable, throwUnrecoverable, UnrecoverableError)
import Control.Monad.TransformerBaseMonad (TransformerBaseMonad, LiftBaseMonad, liftBaseMonad)

import Control.Monad.Unrecoverable (UnrecoverableT)

-- * Allow errors to be combined with state information.

newtype StateErrorT (i :: ErrorInstance) s m a = StateErrorT' (StateT s m a)
  deriving (Functor, Applicative, Monad, MonadTrans)
type M c a m = StateErrorT (ErrorContext c) a m

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
instance WrapError (ErrorState e s) s where
  type Error (ErrorState e s) s = ErrorState e s
  type StateForError (ErrorState e s) s = s
  wrapError' = const
  stateForError = id

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

type SubErrorType :: Type -> Type
type family SubErrorType e where
  SubErrorType (ErrorState e _) = e
instance
  ( MonadUnrecoverable m
  , ErrorState e s ~ UnrecoverableError m
  , Error e s ~ ErrorState e s
  , WrapError e s
  ) => MonadUnrecoverable (StateErrorT i s m) where
  type UnrecoverableError (StateErrorT i s m) = SubErrorType (UnrecoverableError m)
  throwUnrecoverable e = StateErrorT \s -> throwUnrecoverable $ wrapError e s

--instance (MonadUnrecoverable (ErrorState e s) m, WrapError e s, Error e s ~ ErrorState e s) => MonadUnrecoverable e (StateErrorT i s m) where
--  throwUnrecoverable e = StateErrorT \s -> throwUnrecoverable $ wrapError e s
--instance MonadUnrecoverable e m => MonadUnrecoverable e (StateErrorT i s m) where
--  throwUnrecoverable = StateErrorT . throwUnrecoverable

type Test a = StateErrorT 'ErrorStateInstance Int (UnrecoverableT (ErrorState String Int) (Either (ErrorState String Int))) a
test :: Test String
test = do
          i <- get
          when (i > 0) (throwUnrecoverable ("greater" :: String) :: Test ()) <|> fail "should not error"
          when (i == 0) $ fail "zero"
          put $ i + 1
          pure $ show i

