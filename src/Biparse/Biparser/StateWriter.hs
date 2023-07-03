module Biparse.Biparser.StateWriter
  ( Biparser
  , Iso
  , Unit
  , Const
  , ConstU
  , translate
  , zoom
  , runForward
  , evalForward
  , runBackward
  ) where

import Biparse.Biparser (SubState, forward, backward, GetSubState(getSubState), ReplaceSubState(replaceSubState))
import Biparse.Biparser qualified as B

type Biparser c s m n u v = B.Biparser c s (StateT s m) (WriterT (SubState c s) n) u v 
type Iso c m n s v = Biparser c s m n v v
type Unit c s m n = Biparser c s m n () ()
type Const c s m n u = Biparser c s m n u ()
type ConstU c s m n u v = Biparser c s m n u v

-- | Discards unused s' state to avoid commingling m and n monads.
translate :: forall c' c s s' m n ss' u v.
   ( MonadFail m
   , Monad n
   , ss' ~ SubState c' s'
   )
  => Biparser c  s  m n ss' s'
  -> Biparser c' s' m n u   v
  -> Biparser c  s  m n u   v
translate (B.Biparser fw bw) (B.Biparser fw' bw') = B.Biparser
  (StateT \s -> do
    (s',s'') <- runStateT fw s
    (x, _) <- runStateT fw' s'
    pure (x,s'')
  )
  \u -> WriterT do
    (x,w)  <- runWriterT (bw' u)
    (_,w') <- runWriterT (bw  w)
    pure (x,w')

-- | Discards unused s' state to avoid commingling m and n monads.
zoom  :: forall c s s' m n u v ss ss'.
  ( Monad m
  , Monad n
  , ReplaceSubState s' ss s
  , ReplaceSubState s ss' s'
  , GetSubState c s
  , ss' ~ SubState c s'
  , ss ~ SubState c s
  )
  => Iso c m n s ss'
  -> Biparser c s' m n u v
  -> Biparser c s m n u v
zoom (B.Biparser fw bw) (B.Biparser fw' bw') = B.Biparser
  (StateT \s -> do
    (ss,s') <- runStateT fw s
    (x,_) <- runStateT fw' $ replaceSubState s ss
    pure (x,s')
  )
  \u -> WriterT do
    (x,w)  <- runWriterT (bw' u)
    (_,w') <- runWriterT (bw  w)
    pure (x,w')

-- | * Helper run functions

runForward :: forall c s m n u v. Biparser c s m n u v -> s -> m (v, s)
runForward = runStateT . forward

evalForward :: forall c s m n u v. Functor m => Biparser c s m n u v -> s -> m v
evalForward = (fmap fst .) . runForward

runBackward :: forall c s m n u v. Biparser c s m n u v -> u -> n (v, SubState c s)
runBackward = (runWriterT .) . backward

