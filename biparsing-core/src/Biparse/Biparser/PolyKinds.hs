{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE PolyKinds #-}
module Biparse.Biparser.PolyKinds
  ( focus
  ) where

import Biparse.Biparser.Internal (ReplaceSubState(replaceSubState), Iso, Biparser, pattern Biparser)
import Control.Monad.ChangeMonad (ChangeMonad, ChangeFunction, changeMonad')
import Control.Monad.MonadProgenitor (MonadProgenitor)
import Control.Monad.State qualified as S
import Control.Monad.Writer (Writer, execWriter)

-- | Similar to zoom but allows for lazy evaluation of the Iso.
focus :: forall is c' c'' mProgenitor n' m' c s s' m n u v ss ss'.
  -- m
  ( MonadState s m
  -- m'
  , m' ~ MonadProgenitor mProgenitor s'
  , MonadState s' m'
  -- change monad
  , ChangeMonad    is m' m
  , ChangeFunction is m' m ~ (ss -> ss', ss' -> ss)
  , ChangeMonad    is n' n
  , ChangeFunction is n' n ~ (ss' -> ss)
  -- substate
  , ReplaceSubState s ss' s'
  , ReplaceSubState s ss s
  )
  => Iso c'' (S.State s) (Writer ss) s ss'
  -> Biparser c' s' (MonadProgenitor mProgenitor s') n' u v
  -> Biparser c  s  m  n  u v
focus (Biparser fw bw) (Biparser fw' bw') = Biparser
  do
    s <- get
    let (ss',s') = S.runState fw s
        f = S.evalState fw . replaceSubState s
    v <- changeMonad' @is (f,g) do
      put $ replaceSubState s ss'
      fw'
    put s'
    pure v
  \u -> changeMonad' @is g $ bw' u
  where
  g = execWriter . bw

