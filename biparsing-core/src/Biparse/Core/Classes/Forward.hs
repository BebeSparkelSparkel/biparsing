{-# LANGUAGE UndecidableInstances #-}
module Biparse.Core.Classes.Forward (
OneFwd(..),
oneFwd',
StateSeq(StateSeq),
OnlyFwd(..),
Peek(..),
Try(..),
Null(..),
) where

import Data.Sequences (IsSequence(uncons))
import Data.MonoTraversable (Element)
import Biparse.Core.Update (UpdateStateWithElement(updateStateWithElement))
import Biparse.Core.Aliases (LazyStateT, StrictStateT, CPSWriterT, pattern CPSWriterT, LazyWriterT, StrictWriterT, CPSRWST, pattern CPSRWST, LazyRWST, StrictRWST)

class OneFwd a m | m -> a where oneFwd :: m a
deriving instance OneFwd a m => OneFwd a (IdentityT m)
instance (OneFwd a m, Monad m, Monoid w) => OneFwd a (LazyWriterT w m) where oneFwd = lift oneFwd
instance (a ~ Element seq, UpdateStateWithElement s a, IsSequence seq, MonadFail m) => OneFwd a (LazyStateT (StateSeq s seq) m) where oneFwd = oneFwd'
instance (a ~ Element seq, UpdateStateWithElement s a, IsSequence seq, MonadFail m) => OneFwd a (StrictStateT (StateSeq s seq) m) where oneFwd = oneFwd'
instance (a ~ Element seq, UpdateStateWithElement s a, IsSequence seq, MonadFail m, Monoid w) => OneFwd a (CPSRWST r w (StateSeq s seq) m) where oneFwd = oneFwd'
instance (a ~ Element seq, UpdateStateWithElement s a, IsSequence seq, MonadFail m, Monoid w) => OneFwd a (LazyRWST r w (StateSeq s seq) m) where oneFwd = oneFwd'
instance (a ~ Element seq, UpdateStateWithElement s a, IsSequence seq, MonadFail m, Monoid w) => OneFwd a (StrictRWST r w (StateSeq s seq) m) where oneFwd = oneFwd'

oneFwd' ::
  ( IsSequence seq
  , MonadState (StateSeq s seq) m
  , MonadFail m
  , UpdateStateWithElement s a
  , a ~ Element seq
  ) => m a
oneFwd' = do
  StateSeq s xs <- get
  (x, xs') <- maybe (fail "Unexpected end of input.") pure $ uncons xs
  put $ StateSeq (updateStateWithElement x s) xs'
  return x

newtype StateSeq s seq = StateSeq' (s, seq) deriving (Show, Eq)
pattern StateSeq :: s -> seq -> StateSeq s seq
pattern StateSeq s seq = StateSeq' (s, seq)
instance (Default s, IsString seq) => IsString (StateSeq s seq) where fromString = StateSeq def . fromString

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
--peek :: forall c s t m u v.
--  MonadState s m
--  => Biparser c s t m u v
--  -> Biparser c s t m u v
--peek = undefined
--peek (Biparser fw bw) = Biparser
--  (get @s >>= \s -> fw <* put s)
--  bw
instance Peek IO where peek = id
instance Peek Maybe where peek = id
instance Peek Identity where peek = id
instance (Peek m, Monad m) => Peek (IdentityT m) where peek = liftThrough peek
instance (Peek m, Monad m) => Peek (ReaderT r m) where peek = liftThrough peek
instance (Peek m, Monad m, Monoid w) => Peek (CPSWriterT w m) where
  peek (CPSWriterT x) = CPSWriterT $ peek x
instance (Peek m, Monad m, Monoid w) => Peek (LazyWriterT w m) where peek = liftThrough peek
instance (Peek m, Monad m, Monoid w) => Peek (StrictWriterT w m) where peek = liftThrough peek
instance (Peek m, Monad m) => Peek (LazyStateT s m) where peek = peekState
instance (Peek m, Monad m) => Peek (StrictStateT s m) where peek = peekState
instance (Peek m, Monad m, Monoid w) => Peek (CPSRWST r w s m) where
  peek (CPSRWST x) = CPSRWST \r s -> peek $ x r s
instance (Peek m, Monad m, Monoid w) => Peek (LazyRWST r w s m) where peek = peekState
instance (Peek m, Monad m, Monoid w) => Peek (StrictRWST r w s m) where peek = peekState
peekState :: (MonadState s (t m), MonadTransControl t, Peek m, Monad m) => t m a -> t m a
peekState x = get >>= \s -> liftThrough peek x <* put s

-- | Allows trying a forward. If the forward fails the state is returned to the value it was before running.
class Try m where try :: m v -> m v
--try :: forall c s t m u v e.
--  ( MonadError e m
--  , MonadState s m
--  )
--  => Biparser c s t m u v
--  -> Biparser c s t m u v
--try = undefined
--try (Biparser fw bw) = Biparser (tryState fw) bw
--
--tryState :: forall s m v e.
--  ( MonadState s m
--  , MonadError e m
--  )
--  => m v
--  -> m v
--tryState fw = do
--  s <- get @s
--  catchError fw \e -> put s *> throwError e
instance Try IO where try = id
instance (Try m, Monad m) => Try (IdentityT m) where try = liftThrough try
instance (Try m, Monad m) => Try (ReaderT r m) where try = liftThrough try
instance (Try m, Monad m, Monoid w) => Try (CPSWriterT w m) where
  try (CPSWriterT x) = CPSWriterT $ try x
instance (Try m, Monad m, Monoid w) => Try (LazyWriterT w m) where try = liftThrough try
instance (Try m, Monad m, Monoid w) => Try (StrictWriterT w m) where try = liftThrough try
instance (MonadError e m, Try m) => Try (LazyStateT   s m) where try = tryError
instance (MonadError e m, Try m) => Try (StrictStateT s m) where try = tryError
instance (Try m, MonadError e m, Monoid w) => Try (CPSRWST    r w s m) where
  try (CPSRWST x) = CPSRWST \r s -> try $ x r s
instance (MonadError e m, Monoid w, Try m) => Try (LazyRWST   r w s m) where try = tryError
instance (MonadError e m, Monoid w, Try m) => Try (StrictRWST r w s m) where try = tryError
tryError :: forall s t (m :: Type -> Type) e b.
  ( MonadTransControl t
  , MonadState s (t m)
  , MonadError e (t m)
  , Monad m
  , Try m
  ) => t m b -> t m b
tryError x = do
  s <- get
  catchError (liftThrough try x) \e -> do
    put s
    throwError e


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

