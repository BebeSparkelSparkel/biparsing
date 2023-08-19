{-# LANGUAGE AllowAmbiguousTypes #-}
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

import Control.Monad.ChangeMonad (ChangeMonad(ChangeFunction,changeMonad))
import Control.Monad.StateError (StateErrorT, stateErrorT, runStateErrorT, runSET, ResultMonad(ResultingMonad), ErrorContext)
import Biparse.Biparser (SubState, forward, backward, ReplaceSubState(replaceSubState))
import Biparse.Biparser qualified as B

type Biparser c s m n u v = B.Biparser c s (StateErrorT (ErrorContext c) s m) (WriterT (SubState c s) n) u v 
type Iso c m n s v = Biparser c s m n v v
type Unit c s m n = Biparser c s m n () ()
type Const c s m n u = Biparser c s m n u ()
type ConstU c s m n u v = Biparser c s m n u v

type IsoClass c m n a b = B.IsoClass c (StateErrorT (ErrorContext c) a m) (WriterT (SubState c a) n) a b

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
zoom  :: forall is c' c s s' m m' n u v ss'.
  ( Monad m
  , Monad n
  , ReplaceSubState s ss' s'
  , ss' ~ SubState c' s'
  , ChangeMonad is m' m
  , ChangeFunction is m' m ~ ()
  )
  => Iso c m n s ss'
  -> Biparser c' s' m' n u v
  -> Biparser c  s  m n u v
zoom (B.Biparser fw bw) (B.Biparser fw' bw') = B.Biparser
  (stateErrorT \s -> do
    (ss,s') <- (runStateT . runStateErrorT) fw s
    (x,_) <- changeMonad @is () $ (runStateT . runStateErrorT) fw' $ replaceSubState s ss
    pure (x,s')
  )
  \u -> WriterT do
    (x,w)  <- runWriterT (bw' u)
    (_,w') <- runWriterT (bw  w)
    pure (x,w')


----------------------------------------
--gc <- zoom _groupCode $ unless (x == 102) $ fail $ "Expected group code 102 but received group code: " <> show x
---- | 
--something :: forall .
--  ( Eq v
--  )
--  => v -- | Expected value
--  -> (v -> String) -- | Error message
--  -> (s -> v)
--  -> Biparser c s m n () v
--
----------------------------------------
--    v <- zoom (_value . _ValueText) do
--      char '{'
--      rest
--    pure $ CodeValue gc v
---- | 
--something' :: forall .
--  (
--  )
--  => (u -> n v)
--  -> 


-- * Helper run functions

runForward :: forall is c s m n u v.
  ( ChangeMonad is m (ResultingMonad m is)
  , ResultMonad m is
  )
  => Biparser c s m n u v
  -> s
  -> ResultingMonad m is (v, s)
--runForward bp = changeMonad @is (resultMonad @(ResultingMonad m is) @is :: ChangeFunction is (ResultingMonad m is) m') . runSET (forward bp)
runForward bp = runSET @is (forward bp)

evalForward :: forall is c s m m' n u v.
  ( Functor m'
  , ChangeMonad is m m'
  , ResultMonad m is
  , m' ~ ResultingMonad m is
  )
  => Biparser c s m n u v
  -> s
  -> m' v
evalForward = (fmap fst .) . runForward @is

runBackward :: forall c s m n u v. Biparser c s m n u v -> u -> n (v, SubState c s)
runBackward = (runWriterT .) . backward

