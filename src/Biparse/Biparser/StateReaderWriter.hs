{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module Biparse.Biparser.StateReaderWriter
  ( Biparser
  , Iso
  , Unit
  , Const
  , ConstU
  , IsoClass
  , M
  , N
  , zoom
  , zoom'
  , runForward
  , evalForward
  , runBackward
  , runWriterT'
  ) where

import Control.Monad.ChangeMonad (ChangeMonad(ChangeFunction,changeMonad'), ResultMonad(ResultingMonad))
import Control.Monad.StateError (StateErrorT(StateErrorT), runStateErrorT, runSET, M, MonadProgenitor)
import Biparse.Biparser (SubState, forward, backward, ReplaceSubState(replaceSubState))
import Biparse.Biparser qualified as B
import Control.Monad.RWS (RWST(RWST), runRWST)

type Biparser c s m n r ws u v = B.Biparser c s (M c s m) (N c s n r ws) u v 
type Iso c m n r ws s v = Biparser c s m n r ws v v
type Unit c s m n r ws = Biparser c s m n r ws () ()
type Const c s m n r ws u = Biparser c s m n r ws u ()
type ConstU c s m n r ws u v = Biparser c s m n r ws u v

type IsoClass c m n r ws a b = B.IsoClass c (M c a m) (N c a n r ws) a b

type N c s n r ws = RWST r (SubState c s) ws n

zoom :: forall is c' mProgenitor e m' c s s' m n r ws u v ss'.
  ( Monad m
  , Monad n
  , ReplaceSubState s ss' s'
  , ss' ~ SubState c' s'
  , ChangeMonad is m' m
  , ChangeFunction is m' m ~ ()
  -- assignments
  , m  ~ MonadProgenitor mProgenitor e s
  , m' ~ MonadProgenitor mProgenitor e s'
  )
  => Iso c m n r ws s ss'
  -> Biparser c' s' m' n r ws u v
  -> Biparser c  s  m n r ws u v
zoom = zoom' @is @_ @(MonadProgenitor mProgenitor e s') @_ @_ @_ @(MonadProgenitor mProgenitor e s)

-- | Discards unused s' state to avoid commingling m and n monads.
zoom' :: forall is c' m' c s s' m n r ws u v ss'.
  ( Monad m
  , Monad n
  , ReplaceSubState s ss' s'
  , ss' ~ SubState c' s'
  , ChangeMonad is m' m
  , ChangeFunction is m' m ~ ()
  )
  => Iso c m n r ws s ss'
  -> Biparser c' s' m' n r ws u v
  -> Biparser c  s  m n r ws u v
zoom' (B.Biparser fw bw) (B.Biparser fw' bw') = B.Biparser
  (StateErrorT \s -> do
    (ss,s') <- runStateErrorT fw s
    (x,_) <- changeMonad' @is () $ runStateErrorT fw' $ replaceSubState s ss
    pure (x,s')
  )
  \u -> RWST \r s -> do
    (x,s',w)  <- runRWST (bw' u) r s
    (_,s'',w') <- runRWST (bw  w) r s'
    pure (x,s'',w')

-- * Helper run functions

runForward :: forall is c s m rm n r ws u v.
  ( ChangeMonad is m rm
  , ResultMonad m is
  , rm ~ ResultingMonad m is
  )
  => Biparser c s m n r ws u v
  -> s
  -> rm (v, s)
runForward bp = runSET @is (forward bp)

evalForward :: forall is c s m rm n r ws u v.
  ( Functor rm
  , ChangeMonad is m rm
  , ResultMonad m is
  , rm ~ ResultingMonad m is
  )
  => Biparser c s m n r ws u v
  -> s
  -> rm v
evalForward = (fmap fst .) . runForward @is

runBackward :: forall c s m n r ws u v. Functor n => Biparser c s m n r ws u v -> r -> ws -> u -> n (v, SubState c s)
runBackward bp r ws u = runRWST (backward bp u) r ws <&> \(x,_,y) -> (x,y)

runWriterT' :: Functor m => RWST () w () m a -> m (a, w)
runWriterT' x = runRWST x () () <&> \(y,(),z) -> (y,z)

