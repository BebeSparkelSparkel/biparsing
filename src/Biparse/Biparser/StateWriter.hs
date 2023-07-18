module Biparse.Biparser.StateWriter
  ( Biparser
  , Iso
  , Unit
  , Const
  , ConstU
  , IsoClass
  --, translate
  , zoom
  , runForward
  , evalForward
  , runBackward
  , WrapError(..)
  ) where

import Control.Monad.ChangeMonad (ChangeMonad(ChangeFunction,changeMonad))
import Control.Monad.StateError (StateErrorT, stateErrorT, runStateErrorT, runSET, ResultMonad(ResultingMonad,resultMonad))
import Biparse.Error.WrapError (WrapError(wrapError))
import Biparse.Biparser (SubState, forward, backward, ReplaceSubState(replaceSubState))
import Biparse.Biparser qualified as B

type Biparser c s m em n u v = B.Biparser c s (StateErrorT s m) em (WriterT (SubState c s) n) u v 
type Iso c m em n s v = Biparser c s m em n v v
type Unit c s m em n = Biparser c s m em n () ()
type Const c s m em n u = Biparser c s m em n u ()
type ConstU c s m em n u v = Biparser c s m em n u v

type IsoClass c m em n a b = B.IsoClass c (StateErrorT a m) em (WriterT (SubState c a) n) a b

---- | Discards unused s' state to avoid commingling m and n monads.
--translate :: forall c' c s s' m m' n ss' u v.
--   ( Monad m
--   , Monad n
--   , ss' ~ SubState c' s'
--   )
--  => (forall a. StateErrorT c' s' m' a -> StateErrorT c s m a)
--  -> Biparser c  s  m  n ss' s'
--  -> Biparser c' s' m' n u   v
--  -> Biparser c  s  m  n u   v
--translate f (B.Biparser fw bw) (B.Biparser fw' bw') = B.Biparser
--  (stateErrorT \s -> do
--    (s',s'') <- runSET fw s
--    (x, _) <- f $ runSET fw' s'
--    pure (x,s'')
--  )
--  \u -> WriterT do
--    (x,w)  <- runWriterT (bw' u)
--    (_,w') <- runWriterT (bw  w)
--    pure (x,w')

-- | Discards unused s' state to avoid commingling m and n monads.
zoom  :: forall c' c s s' m em n u v ss'.
  ( Monad m
  , Monad n
  , ReplaceSubState s ss' s'
  , ss' ~ SubState c' s'
  )
  => Iso c m em n s ss'
  -> Biparser c' s' m em n u v
  -> Biparser c  s  m em n u v
zoom (B.Biparser fw bw) (B.Biparser fw' bw') = B.Biparser
  (stateErrorT \s -> do
    (ss,s') <- (runStateT . runStateErrorT) fw s
    (x,_) <- (runStateT . runStateErrorT) fw' $ replaceSubState s ss
    pure (x,s')
  )
  \u -> WriterT do
    (x,w)  <- runWriterT (bw' u)
    (_,w') <- runWriterT (bw  w)
    pure (x,w')

-- * Helper run functions

type Forward m m' =
  ( ChangeMonad (ResultingMonad m) m'
  , ChangeMonad m (ResultingMonad m)
  , ResultMonad m
  , ResultMonad (ResultingMonad m)
  --, ChangeFunction (ResultingMonad m) m' ~ ChangeFunction (ResultingMonad m) (ResultingMonad (ResultingMonad m))
  , m' ~ ResultingMonad (ResultingMonad m)
  )

runForward :: forall c s m m' em n u v.
  Forward m m'
  => Biparser c s m em n u v
  -> s
  -> m' (v, s)
runForward bp = changeMonad (resultMonad @(ResultingMonad m) :: ChangeFunction (ResultingMonad m) m') . runSET (forward bp)

evalForward :: forall c s m m' em n u v.
  ( Forward m m'
  , Functor m'
  )
  => Biparser c s m em n u v
  -> s
  -> m' v
evalForward = (fmap fst .) . runForward

runBackward :: forall c s m em n u v. Biparser c s m em n u v -> u -> n (v, SubState c s)
runBackward = (runWriterT .) . backward

