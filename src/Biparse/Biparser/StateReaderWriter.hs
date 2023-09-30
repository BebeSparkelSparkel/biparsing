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
import Control.Monad.StateError (StateErrorT, stateErrorT, runStateErrorT, runSET, ResultMonad(ResultingMonad), ErrorContext)
import Biparse.Biparser (SubState, forward, backward, ReplaceSubState(replaceSubState))
import Biparse.Biparser qualified as B
import Control.Monad.RWS (RWST(RWST), runRWST)

type Biparser c s m n r u v = B.Biparser c s (M c s m) (N c s n r) u v 
type Iso c m n r s v = Biparser c s m n r v v
type Unit c s m n r = Biparser c s m n r () ()
type Const c s m n r u = Biparser c s m n r u ()
type ConstU c s m n r u v = Biparser c s m n r u v

type IsoClass c m n r a b = B.IsoClass c (M c a m) (N c a n r) a b

type M c s m = StateErrorT (ErrorContext c) s m
type N c s n r = RWST r (SubState c s) () n

-- | Discards unused s' state to avoid commingling m and n monads.
-- c' m'
zoom  :: forall is c' m' c s s' m n r u v ss'.
  ( Monad m
  , Monad n
  , ReplaceSubState s ss' s'
  , ss' ~ SubState c' s'
  , ChangeMonad is m' m
  , ChangeFunction is m' m ~ ()
  )
  => Iso c m n r s ss'
  -> Biparser c' s' m' n r u v
  -> Biparser c  s  m n r u v
zoom (B.Biparser fw bw) (B.Biparser fw' bw') = B.Biparser
  (stateErrorT \s -> do
    (ss,s') <- (runStateT . runStateErrorT) fw s
    (x,_) <- changeMonad' @is () $ (runStateT . runStateErrorT) fw' $ replaceSubState s ss
    pure (x,s')
  )
  \u -> RWST \r s -> do
    (x,s',w)  <- runRWST (bw' u) r s
    (_,s'',w') <- runRWST (bw  w) r s'
    pure (x,s'',w')

-- * Helper run functions

runForward :: forall is c s m n r u v.
  ( ChangeMonad is m (ResultingMonad m is)
  , ResultMonad m is
  )
  => Biparser c s m n r u v
  -> s
  -> ResultingMonad m is (v, s)
runForward bp = runSET @is (forward bp)

evalForward :: forall is c s m m' n r u v.
  ( Functor m'
  , ChangeMonad is m m'
  , ResultMonad m is
  , m' ~ ResultingMonad m is
  )
  => Biparser c s m n r u v
  -> s
  -> m' v
evalForward = (fmap fst .) . runForward @is

runBackward :: forall c s m n r u v. Functor n => Biparser c s m n r u v -> r -> u -> n (v, SubState c s)
runBackward bp r u = runRWST (backward bp u) r () <&> \(x,_,y) -> (x,y)

runWriterT' :: Functor m => RWST () w () m a -> m (a, w)
runWriterT' x = runRWST x () () <&> \(y,_,z) -> (y,z)

