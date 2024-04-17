{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP #-}
module Biparse.Biparser.StateReaderWriter
  ( Biparser
  , Iso
  , Unit
  , Const
  , ConstU
  , IsoClass
  , M
  , N
  , BackwardC(..)
  , BackwardArgC
  , BackwardArg
  , zoom
  , zoomWrite
  , zoomOne
  , runForward
  , evalForward
  , runBackward
  , evalBackward
  , runWriterT'
  ) where

import Biparse.Biparser (forward, backward, ReplaceSubState(replaceSubState), oneFw)
import Biparse.Biparser qualified as B
import Control.Monad.ChangeMonad (ChangeMonad, changeMonad')
import Control.Monad.MonadProgenitor (MonadProgenitor)
import Control.Monad.StateError (StateErrorT(StateErrorT), runStateErrorT, M, ErrorContext)

type Biparser c s m n r w ws = B.Biparser c s (M s m) (N c n r w ws)
type Iso c m n r w ws s v = Biparser c s m n r w ws v v
type Unit c s m n r w ws = Biparser c s m n r w ws () ()
type Const c s m n r w ws u = Biparser c s m n r w ws u ()
type ConstU c s m n r w ws u v = Biparser c s m n r w ws u v

type IsoClass c m n r w ws a b = B.IsoClass c (M a m) (N c n r w ws) a b

type N c n r w ws = BackwardT c r w ws n

class BackwardC c n r w s where
  type BackwardT c :: Type -> Type -> Type -> (Type -> Type) -> Type -> Type
  backwardT :: forall a. (r -> s -> n (a, s, w)) -> BackwardT c r w s n a
  runBackwardT :: forall a. BackwardT c r w s n a -> BackwardArgC c -> r -> s -> n (a, s, w)

type BackwardArgC c = BackwardArg (BackwardT c)
type BackwardArg :: (Type -> Type -> Type -> (Type -> Type) -> Type -> Type) -> Type
type family BackwardArg t

-- | Strict on the Iso which makes 'Biparser' slow to run if not all 'ss\'' is requred and for error to be thrown.
zoom :: forall is c' mProgenitor w m' c ss' s s' m n r ws u v.
  ( Monad m
  , Monad n
  , ReplaceSubState s ss' s'
  , ChangeMonad is m' m ()
  --, ChangeFunction is m' m ~ ()
  --
  , BackwardC c  n r w   ws
  , BackwardC c' n r ss' ws
  -- Backward
  , Default (BackwardArgC c)
  , Default (BackwardArgC c')
  -- assignments
  , m  ~ MonadProgenitor mProgenitor s
  , m' ~ MonadProgenitor mProgenitor s'
  )
  => Iso c m n r w ws s ss'
  -> Biparser c' s' (MonadProgenitor mProgenitor s') n r ss' ws u v
  -> Biparser c  s  (MonadProgenitor mProgenitor s)  n r w  ws u v
zoom (B.Biparser fw bw) (B.Biparser fw' bw') = B.Biparser
  (StateErrorT \s -> do
    (ss,s') <- runStateErrorT fw s
    (x,_) <- changeMonad' @is () $ runStateErrorT fw' $ replaceSubState s ss
    pure (x,s')
  )
  \u -> backwardT @c \r s -> do
    (x,s',w)   <- runBackwardT @c' (bw' u)  def r s
    (_,s'',w') <- runBackwardT @c  (bw $ w) def r s'
    pure (x,s'',w')

-- | Strict on the Iso which makes 'Biparser' slow to run if not all 'ss\'' is requred and for error to be thrown.
zoomWrite :: forall is c' mProgenitor w m' c ss' s s' m n r ws u v.
  -- m
  ( Monad m
  , ChangeMonad is m' m ()
  --, ChangeFunction is m' m ~ ()
  -- n
  , Monad n
  , BackwardC c  n r w ws
  , BackwardC c' n r w ws
  -- substate
  , ReplaceSubState s ss' s'
  -- w
  , Monoid w
  -- Backward
  , Default (BackwardArgC c)
  , Default (BackwardArgC c')
  -- assignments
  , m  ~ MonadProgenitor mProgenitor s
  , m' ~ MonadProgenitor mProgenitor s'
  )
  -- => Iso c m n r w ws s ss'
  => Biparser c s m n r w ws u ss'
  -> Biparser c' s' (MonadProgenitor mProgenitor s') n r w ws u v
  -> Biparser c  s  (MonadProgenitor mProgenitor s)  n r w  ws u v
zoomWrite (B.Biparser fw bw) (B.Biparser fw' bw') = B.Biparser
  (StateErrorT \s -> do
    (ss,s') <- runStateErrorT fw s
    (x,_) <- changeMonad' @is () $ runStateErrorT fw' $ replaceSubState s ss
    pure (x,s')
  )
  \u -> backwardT @c \r s -> do
    (x,s',w)   <- runBackwardT @c' (bw' u) def r s
    (_,s'',w') <- runBackwardT @c  (bw  u) def r s'
    pure (x,s'', w <> w')

-- | Strict on the Iso which makes 'Biparser' slow to run if not all 'ss\'' is requred and for error to be thrown.
zoomOne :: forall is c' mProgenitor w m' c s s' m n r ws u v i.
  -- m
  ( Monad m
  , Alt m
  , ChangeMonad is m' m ()
  --, ChangeFunction is m' m ~ ()
  , MonadFail (StateErrorT i s m)
  -- n
  , Monad n
  , BackwardC c  n r w ws
  , BackwardC c' n r (Element w) ws
  -- substate
  , IsSequence (B.SubState s)
  , ReplaceSubState s (Element (B.SubState s)) s'
  , B.ElementContext c s
  -- w
  , MonoPointed w
  -- Backward
  , Default (BackwardArgC c')
  -- assignments
  , m  ~ MonadProgenitor mProgenitor s
  , m' ~ MonadProgenitor mProgenitor s'
  , i ~ ErrorContext m
  )
  => Biparser c' s' (MonadProgenitor mProgenitor s') n r (Element w) ws u v
  -> Biparser c  s  (MonadProgenitor mProgenitor s)  n r w           ws u v
zoomOne (B.Biparser fw' bw') = B.Biparser
  (StateErrorT \s -> do
    (ss,s') <- runStateErrorT @i (oneFw @c) s
    (x,_) <- changeMonad' @is () $ runStateErrorT fw' $ replaceSubState s ss
    pure (x,s')
  )
  \u -> backwardT @c \r s -> do
    (x,s',w) <- runBackwardT @c' (bw' u) def r s
    pure (x,s', singleton w)

-- * Helper run functions

runForward :: forall c s m n r w ws u v.
  ( -- ChangeMonad is m rm
  --, ResultMonad m is
  --, rm ~ ResultingMonad m is
  )
  => Biparser c s m n r w ws u v
  -> s
  -> m (v, s)
runForward bp = runStateErrorT (forward bp)

evalForward :: forall c s m n r w ws u v.
  ( Functor m
  --, ChangeMonad is m rm
  --, ResultMonad m is
  --, rm ~ ResultingMonad m is
  )
  => Biparser c s m n r w ws u v
  -> s
  -> m v
evalForward = (fmap fst .) . runForward

runBackward :: forall c s m n r w ws u v.
  ( Functor n
  , BackwardC c n r w ws
  )
  => Biparser c s m n r w ws u v
  -> BackwardArgC c
  -> r
  -> ws
  -> u
  -> n (v, w)
runBackward bp ba r ws u = runBackwardT @c (backward bp u) ba r ws <&> \(x,_,y) -> (x,y)

evalBackward :: forall c s m n r w ws u v.
  ( Functor n
  , BackwardC c n r w ws
  )
  => Biparser c s m n r w ws u v
  -> BackwardArgC c
  -> r
  -> ws
  -> u
  -> n w
evalBackward bp ba r ws u = snd <$> runBackward @c bp ba r ws u

runWriterT' :: forall c n w a.
  ( Functor n
  , BackwardC c n () w ()
  , Default (BackwardArgC c)
  ) => BackwardT c () w () n a -> n (a, w)
runWriterT' x = runBackwardT @c x def () () <&> \(y,(),z) -> (y,z)

