module Biparse.Core.Classes.Forward (
OnlyFwd(..),
Peek(..),
Try(..),
Null(..),
) where

import Biparse.Core.Aliases (CPSWriterT, pattern CPSWriterT, LazyWriterT, StrictWriterT, LazyStateT, StrictStateT, CPSRWST, pattern CPSRWST, LazyRWST, StrictRWST)

-- * Forward and Backward Divergence

class OnlyFwd m where onlyFwd :: m () -> m ()

--forwardFail :: forall p u.
--  ( MonadFail p
--  )
--  => Biparser p u ()
--forwardFail = undefined
----forwardFail = Biparser (fail "Purposely forward fail.") (const $ pure ())
--
---- | Throws away the forward computation and returns 'x'. Only the backwards computation runs.
--ignoreForward :: forall p u v.
--  Applicative p
--  => v
--  -> Biparser p u v
--  -> Biparser p u v
--ignoreForward = undefined
----ignoreForward x y = setForward y $ pure x
--

-- | Modifies forward so that the Biparser does not consume input nor modify the state.
class Peek m where peek :: m v -> m v
instance Peek IO where peek = id
instance Peek Maybe where peek = id
instance Peek Identity where peek = id
instance (Peek m, Monad m) => Peek (IdentityT m) where peek = liftThrough peek
instance (Peek m, Monad m) => Peek (ReaderT r m) where peek = liftThrough peek
instance (Peek m, Monad m, Monoid w) => Peek (CPSWriterT w m) where
  peek (CPSWriterT x) = CPSWriterT $ peek x
instance (Peek m, Monad m, Monoid w) => Peek (LazyWriterT w m) where peek = liftThrough peek
instance (Peek m, Monad m, Monoid w) => Peek (StrictWriterT w m) where peek = liftThrough peek
instance (Peek m, Monad m) => Peek (LazyStateT s m) where peek = liftThrough peek
instance (Peek m, Monad m) => Peek (StrictStateT s m) where peek = liftThrough peek
instance (Peek m, Monad m, Monoid w) => Peek (CPSRWST r w s m) where
  peek (CPSRWST x) = CPSRWST \r s -> peek $ x r s
instance (Peek m, Monad m, Monoid w) => Peek (LazyRWST r w s m) where peek = liftThrough peek
instance (Peek m, Monad m, Monoid w) => Peek (StrictRWST r w s m) where peek = liftThrough peek

-- | Allows trying a forward. If the forward fails the state is returned to the value it was before running.
class Try m where try :: m v -> m v
instance Try IO where try = id
instance Try Maybe where try = id
instance (Try m, Monad m) => Try (IdentityT m) where try = liftThrough try
instance (Try m, Monad m) => Try (ReaderT r m) where try = liftThrough try
instance (Try m, Monad m, Monoid w) => Try (CPSWriterT w m) where
  try (CPSWriterT x) = CPSWriterT $ try x
instance (Try m, Monad m, Monoid w) => Try (LazyWriterT w m) where try = liftThrough try
instance (Try m, Monad m, Monoid w) => Try (StrictWriterT w m) where try = liftThrough try
instance (Try m, Monad m) => Try (LazyStateT   s m) where try = liftThrough try
instance (Try m, Monad m) => Try (StrictStateT s m) where try = liftThrough try
instance (Try m, Monoid w, Functor m) => Try (CPSRWST    r w s m) where
  try (CPSRWST x) = CPSRWST \r s -> try $ x r s
instance (Try m, Monoid w, Monad m) => Try (LazyRWST   r w s m) where try = liftThrough try
instance (Try m, Monoid w, Monad m) => Try (StrictRWST   r w s m) where try = liftThrough try

-- | Allows back to not execute and return 'x' if 'f' returns 'Nothing'
--optionalBack :: forall c s t m u u' v.
--  ()
--  => (u -> Maybe u')
--  -> v
--  -> Biparser c s t m u' v
--  -> Biparser c s t m u  v
--optionalBack = undefined
--optionalBack f x (Biparser fw bw) = Biparser fw $ maybe (pure x) bw . f

-- | End Dectection
class Null m where
  -- | Returns true if the substate is empty.
  null :: m bool
  -- fails if not at the end of the input
  eof :: m ()
  
-- | Returns true if the substate is empty.
-- DEV NOTE: May be able to be written in general without Biparser constructor
--isNull :: forall c s t m u ss.
--  ( MonoFoldable u
--  , GetSubState s
--  , MonadState s m
--  , MonoFoldable ss
--  , ss ~ SubState s
--  )
--  => Biparser c s t m u Bool
--isNull = undefined
--isNull = Biparser subStateNull (pure . null)

-- fails if not at the end of the input
--eof ::
--  ( MonadState s m
--  , MonadFail m
--  , GetSubState s
--  , MonoFoldable (SubState s)
--  ) => Const c s t m u
--eof = undefined
--eof = Biparser eofFw $ const $ pure ()
--
--eofFw ::
--  ( MonadState s m
--  , MonadFail m
--  , GetSubState s
--  , MonoFoldable (SubState s)
--  ) => m ()
--eofFw = whenM subStateNull (fail "Expected to be at the end of input but there is still more")

---- * failure
--
--failForward :: forall c s m n u v.
--  MonadFail m
--  => Biparser m u v
--  -> Biparser m u v
--failForward = undefined
----failForward = firstM $ const $ fail "pureposely failed in the forwards direction"
--

