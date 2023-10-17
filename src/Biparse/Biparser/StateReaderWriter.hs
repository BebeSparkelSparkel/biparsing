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
  , runForward
  , evalForward
  , runBackward
  , runWriterT'
  ) where

import Control.Monad.ChangeMonad (ChangeMonad(ChangeFunction,changeMonad'))
import Control.Monad.StateError (StateErrorT(StateErrorT), runStateErrorT, runSET, ResultMonad(ResultingMonad), ErrorContext)
import Biparse.Biparser (SubState, forward, backward, ReplaceSubState(replaceSubState))
import Biparse.Biparser qualified as B
import Control.Monad.RWS (RWST(RWST), runRWST)

type Biparser c s m n r ws u v = B.Biparser c s (M c s m) (N c s n r ws) u v 
type Iso c m n r ws s v = Biparser c s m n r ws v v
type Unit c s m n r ws = Biparser c s m n r ws () ()
type Const c s m n r ws u = Biparser c s m n r ws u ()
type ConstU c s m n r ws u v = Biparser c s m n r ws u v

type IsoClass c m n r ws a b = B.IsoClass c (M c a m) (N c a n r ws) a b

type M c s m = StateErrorT (ErrorContext c) s m
type N c s n r ws = RWST r (SubState c s) ws n

-- | Discards unused s' state to avoid commingling m and n monads.
zoom  :: forall is c' m' c s s' m n r ws u v ss'.
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
zoom (B.Biparser fw bw) (B.Biparser fw' bw') = B.Biparser
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

runForward :: forall is c s m n r ws u v.
  ( ChangeMonad is m (ResultingMonad m is)
  , ResultMonad m is
  )
  => Biparser c s m n r ws u v
  -> s
  -> ResultingMonad m is (v, s)
runForward bp = runSET @is (forward bp)

evalForward :: forall is c s m m' n r ws u v.
  ( Functor m'
  , ChangeMonad is m m'
  , ResultMonad m is
  , m' ~ ResultingMonad m is
  )
  => Biparser c s m n r ws u v
  -> s
  -> m' v
evalForward = (fmap fst .) . runForward @is

runBackward :: forall c s m n r ws u v. Functor n => Biparser c s m n r ws u v -> r -> ws -> u -> n (v, SubState c s)
runBackward bp r ws u = runRWST (backward bp u) r ws <&> \(x,_,y) -> (x,y)

runWriterT' :: Functor m => RWST () w () m a -> m (a, w)
runWriterT' x = runRWST x () () <&> \(y,_,z) -> (y,z)

