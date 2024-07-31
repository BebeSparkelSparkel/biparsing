{-# LANGUAGE UndecidableInstances #-}
module Biparse.Core.Classes.Forward (
OneFwd(..),
oneFwd',
OnlyFwd(..),
Peek(..),
Try(..),
try',
Null(..),
) where

import Control.Monad.State (StateT, MonadState(get,put))
import Data.Sequences (IsSequence(uncons))
import Data.MonoTraversable (Element)
import Biparse.Core.Update (UpdateStateWithElement(updateStateWithElement))

class OneFwd a m | m -> a where oneFwd :: m a

instance (a ~ Element seq, UpdateStateWithElement s a, IsSequence seq, MonadFail m) => OneFwd a (StateT (s, seq) m) where
  oneFwd = oneFwd'

oneFwd' ::
  ( IsSequence seq
  , MonadState (s, seq) m
  , MonadFail m
  , UpdateStateWithElement s a
  , a ~ Element seq
  ) => m a
oneFwd' = do
  (s, xs) <- get
  (x, xs') <- maybe (fail "Unexpected end of input.") pure $ uncons xs
  put (updateStateWithElement x s, xs')
  return x

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
instance Monad m => Peek (StateT s m) where
  peek x = do
    s <- get
    y <- x
    put s
    return y
instance Peek IO where
  peek = id

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
instance MonadError e m => Try (StateT s m) where
  try = try'

try' :: (MonadState s m, MonadError e m) => m b -> m b
try' x = do
  s <- get
  catchError x \e -> do
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

