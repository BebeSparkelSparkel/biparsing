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
  ) where

import Control.Monad.StateError (StateErrorT, runSET, stateErrorT)
import Biparse.Biparser (SubState, forward, backward, ReplaceSubState(replaceSubState))
import Biparse.Biparser qualified as B

type Biparser c s m n u v = B.Biparser c s (StateErrorT c s m) (WriterT (SubState c s) n) u v 
type Iso c m n s v = Biparser c s m n v v
type Unit c s m n = Biparser c s m n () ()
type Const c s m n u = Biparser c s m n u ()
type ConstU c s m n u v = Biparser c s m n u v

type IsoClass c m n a b = B.IsoClass c (StateT a m) (WriterT (SubState c a) n) a b

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
zoom  :: forall c' c s s' m n u v ss'.
  ( Monad m
  , Monad n
  , ReplaceSubState s ss' s'
  , ss' ~ SubState c' s'
  )
  => Iso c m n s ss'
  -> Biparser c' s' m n u v
  -> Biparser c  s  m n u v
zoom (B.Biparser fw bw) (B.Biparser fw' bw') = B.Biparser
  (stateErrorT \s -> do
    (ss,s') <- runSET fw s
    (x,_) <- runSET fw' $ replaceSubState s ss
    pure (x,s')
  )
  \u -> WriterT do
    (x,w)  <- runWriterT (bw' u)
    (_,w') <- runWriterT (bw  w)
    pure (x,w')

-- * Helper run functions

runForward :: forall c s m n u v. Biparser c s m n u v -> s -> m (v, s)
runForward = runSET . forward

evalForward :: forall c s m n u v.
  ( Functor m
  )
  => Biparser c s m n u v
  -> s
  -> m v
evalForward = (fmap fst .) . runForward

runBackward :: forall c s m n u v. Biparser c s m n u v -> u -> n (v, SubState c s)
runBackward = (runWriterT .) . backward

