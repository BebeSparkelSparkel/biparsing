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
  , zoom
  , runForward
  , evalForward
  , runBackward
  , evalBackward
  , runWriterT'
  ) where

import Biparse.Biparser (forward, backward, ReplaceSubState(replaceSubState))
import Biparse.Biparser qualified as B
import Control.Monad.ChangeMonad (ChangeMonad, ChangeFunction, changeMonad', ResultMonad(ResultingMonad))
import Control.Monad.MonadProgenitor (MonadProgenitor)
import Control.Monad.StateError (StateErrorT(StateErrorT), runStateErrorT, runSET, M)

type Biparser c s m n r w ws u v = B.Biparser c s (M c s m) (N c s n r w ws) u v 
type Iso c m n r w ws s v = Biparser c s m n r w ws v v
type Unit c s m n r w ws = Biparser c s m n r w ws () ()
type Const c s m n r w ws u = Biparser c s m n r w ws u ()
type ConstU c s m n r w ws u v = Biparser c s m n r w ws u v

type IsoClass c m n r w ws a b = B.IsoClass c (M c a m) (N c a n r w ws) a b

type N c s n r w ws = BackwardT c r w ws n

class BackwardC c where
  type BackwardT c :: Type -> Type -> Type -> (Type -> Type) -> Type -> Type
  backwardT :: forall n r w s a. (r -> s -> n (a, s, w)) -> BackwardT c r w s n a
  runBackwardT   :: forall n r w s a. BackwardT c r w s n a -> r -> s -> n (a, s, w)

-- | Strict on the Iso which makes 'Biparser' slow to run if not all 'ss\'' is requred and for error to be thrown.
zoom :: forall is c' mProgenitor m' c ss' s s' m n r w ws u v.
  ( Monad m
  , Monad n
  , ReplaceSubState s ss' s'
  , ChangeMonad is m' m
  , ChangeFunction is m' m ~ ()
  --
  , BackwardC c
  , BackwardC c'
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
    (x,s',w)   <- runBackwardT @c' (bw' u)  r s
    (_,s'',w') <- runBackwardT @c  (bw $ w) r s'
    pure (x,s'',w')

--stripSuperState :: forall mProgenitor c s m m' n r ws u v ss.
--  ( Monad (MonadProgenitor mProgenitor s)
--  , ss ~ SubState s
--  )
--  => Biparser IdentityState ss (M IdentityState ss (MonadProgenitor mProgenitor ss)) n r ws u v
--  -> Biparser c  s  (M c s(MonadProgenitor mProgenitor s))  n r ws u v
--stripSuperState (B.Biparser (StateErrorT fw) bw) = B.Biparser
--  do
--    s <- get
--    (x,ss) <- fw $ getSubState @c s
--    put $ replaceSubState s ss
--    pure x
--  bw

-- * Helper run functions

runForward :: forall is c s m rm n r w ws u v.
  ( ChangeMonad is m rm
  , ResultMonad m is
  , rm ~ ResultingMonad m is
  )
  => Biparser c s m n r w ws u v
  -> s
  -> rm (v, s)
runForward bp = runSET @is (forward bp)

evalForward :: forall is c s m rm n r w ws u v.
  ( Functor rm
  , ChangeMonad is m rm
  , ResultMonad m is
  , rm ~ ResultingMonad m is
  )
  => Biparser c s m n r w ws u v
  -> s
  -> rm v
evalForward = (fmap fst .) . runForward @is

runBackward :: forall c s m n r w ws u v.
  ( Functor n
  , BackwardC c
  ) => Biparser c s m n r w ws u v -> r -> ws -> u -> n (v, w)
runBackward bp r ws u = runBackwardT @c (backward bp u) r ws <&> \(x,_,y) -> (x,y)

evalBackward :: forall c s m n r w ws u v.
  ( Functor n
  , BackwardC c
  ) => Biparser c s m n r w ws u v -> r -> ws -> u -> n w
evalBackward bp r ws u = snd <$> runBackward @c bp r ws u

runWriterT' :: forall c m w a.
  ( Functor m
  , BackwardC c
  ) => BackwardT c () w () m a -> m (a, w)
runWriterT' x = runBackwardT @c x () () <&> \(y,(),z) -> (y,z)

