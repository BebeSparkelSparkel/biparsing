module Biparse.Control.BP (BP(..)) where

-- | Type indicator that the biparser action should be executed in @m@.
newtype BP m a = BP {runBP :: m a}
  deriving (Functor, Applicative, Alternative, Monad, MonadFail)
deriving instance MonadState s m => MonadState s (BP m)

instance (Peek m, Monad m) => Peek (BP (LazyStateT s m)) where peek = peekState
instance (Peek m, Monad m) => Peek (BP (StrictStateT s m)) where peek = peekState
instance (Peek m, Monad m, Monoid w) => Peek (BP (CPSRWST r w s m)) where
  peek (BP (CPSRWST x)) = BP $ CPSRWST \r s -> peek $ x r s
instance (Peek m, Monad m, Monoid w) => Peek (BP (LazyRWST r w s m)) where peek = peekState
instance (Peek m, Monad m, Monoid w) => Peek (BP (StrictRWST r w s m)) where peek = peekState
peekState :: (MonadState s (t m), MonadTransControl t, Peek m, Monad m) => BP (t m) a -> BP (t m) a
peekState (BP x) = BP $ get >>= \s -> liftThrough peek x <* put s

instance (OnError m, Try m) => Try (BP (LazyStateT   s m)) where try = tryError
instance (OnError m, Try m) => Try (BP (StrictStateT s m)) where try = tryError
instance (OnError m, Monoid w, Try m) => Try (BP (CPSRWST   r w s m)) where
  try (BP (CPSRWST x)) = BP $ onError (CPSRWST \r s -> try (x r s)) . put =<< get
instance (OnError m, Monoid w, Try m) => Try (BP (LazyRWST   r w s m)) where try = tryError
instance (OnError m, Monoid w, Try m) => Try (BP (StrictRWST   r w s m)) where try = tryError
tryError :: forall s t (m :: Type -> Type) b.
  ( MonadTransControl t
  , MonadState s (t m)
  , OnError (t m)
  , Monad m
  , Try m
  ) => BP (t m) b -> BP (t m) b
tryError (BP x) = BP $ onError (liftThrough try x) . put =<< get
