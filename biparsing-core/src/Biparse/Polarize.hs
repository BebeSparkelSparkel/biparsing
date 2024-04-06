{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE PolyKinds #-}
module Biparse.Polarize
  ( Polar
  -- , polarize
  ) where

import Biparse.Biparser.Internal (ReplaceSubState(replaceSubState), Iso, Biparser, pattern Biparser, SubState)
import Control.Monad.ChangeMonad (ChangeMonad, changeMonad')
import Control.Monad.MonadProgenitor (MonadProgenitor)
import Control.Monad.State (State, runState, evalState)
import Control.Monad.Writer (Writer, execWriter)

import GHC.Err (undefined)

type Polar c s ss' = Iso c (State s) (Writer (SubState s)) s ss'

---- | Similar to zoom but allows for lazy evaluation of the Iso.
--polarize :: forall is c' c'' n' m' c s s' m n u v ss ss'.
--  -- m
--  ( MonadState s m
--  -- m'
--  , MonadState s' m'
--  -- substate
--  , ReplaceSubState s ss' s'
--  , ReplaceSubState s ss s
--  )
--  => Polar c'' s ss'
--  -> Biparser c' s' m' n' u v
--  -> Biparser c  s  m  n  u v
--polarize (Biparser fw bw) (Biparser fw' bw') = Biparser
--  do
--    s <- get
--    let (ss',s') = runState fw s
--    
--  do
--    s <- get
--    let (ss',s') = runState fw s
--        f = evalState fw . replaceSubState s
--    v <- changeMonad' @is (f,g) do
--      put $ replaceSubState s ss'
--      fw'
--    put s'
--    pure v
--  \u -> changeMonad' @is g $ bw' u
--  where
--  g = execWriter . bw

