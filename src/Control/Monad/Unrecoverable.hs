{-# LANGUAGE UndecidableInstances #-}
module Control.Monad.Unrecoverable
  ( UnrecoverableT(..)
  , evalUnrecoverableT
  , MonadUnrecoverable(..)
  ) where

-- * UnrecoverableT

newtype UnrecoverableT e m a = UnrecoverableT {runUnrecoverableT :: m (Either e a)}
  deriving (Functor)

instance Monad m => Applicative (UnrecoverableT e m) where
  pure = UnrecoverableT . pure . Right
  UnrecoverableT f <*> UnrecoverableT x = UnrecoverableT $ f >>= \case
    Right f' -> x >>= pure . \case
      Right x' -> Right $ f' x'
      Left e -> Left e
    Left e -> pure $ Left e

instance (Monad m, Alternative m) => Alternative (UnrecoverableT e m) where
  empty = UnrecoverableT empty
  UnrecoverableT x <|> UnrecoverableT y = UnrecoverableT $ x <|> y

instance (Monad m, Alternative m) => MonadPlus (UnrecoverableT e m)

instance Monad m => Monad (UnrecoverableT e m) where
  UnrecoverableT x >>= f = UnrecoverableT $ x >>= \case
    Right x' -> runUnrecoverableT $ f x'
    Left e -> pure $ Left e

instance MonadFail m => MonadFail (UnrecoverableT e m) where
  fail = UnrecoverableT . fail

instance MonadError e m => MonadError e (UnrecoverableT e m) where
  throwError = UnrecoverableT . throwError
  catchError (UnrecoverableT x) f = UnrecoverableT $ catchError x $ runUnrecoverableT . f

-- * Running 

evalUnrecoverableT :: MonadError e m => UnrecoverableT e m a -> m a
evalUnrecoverableT = either throwError pure <=< runUnrecoverableT

-- * MonadUnrecoverable

class MonadUnrecoverable m where
  type UnrecoverableError m :: Type
  throwUnrecoverable :: UnrecoverableError m -> m a

instance Applicative m => MonadUnrecoverable (UnrecoverableT e m) where
  type UnrecoverableError (UnrecoverableT e _) = e
  throwUnrecoverable = UnrecoverableT . pure . Left

instance (MonadUnrecoverable m, Monad m) => MonadUnrecoverable (StateT s m) where
  type UnrecoverableError (StateT _ m) = UnrecoverableError m
  throwUnrecoverable = lift . throwUnrecoverable

instance (MonadUnrecoverable m, Monoid w, Monad m) => MonadUnrecoverable (RWST r w s m) where
  type UnrecoverableError (RWST _ _ _ m) = UnrecoverableError m
  throwUnrecoverable = lift . throwUnrecoverable

