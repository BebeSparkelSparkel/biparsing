{-# LANGUAGE PolyKinds #-}
module Biparse.Core.Classes.Bidirectional (
Item',
One(one),
BiN(biN),
Diverge(..),
) where

import Biparse.Core.Aliases (Iso)

type Item' :: k -> Type
type family Item' a

-- | Returns one element.
class One a m | m -> a where
  one :: Iso m a

-- | Returns n elements.
-- When writing (backwards) all characters should probably be writtern not just N.
class BiN m where
  biN :: Int -> Iso m seq

-- * Forward and Backward Divergence

class Diverge m f b u | m -> f b, b -> u where
  -- | First argument is run forward and the second is run backwards
  diverge :: f a -> (u -> b a) -> m a

---- | Takes and writes one element. Updates the context and substate.
--one :: forall w c s t m ss se.
--  ( IsSequence ss
--  , ElementContext c s
--  -- m
--  , MonadState s m
--  , MonadFail m
--  , Alternative m
--  -- w
--  -- assignments
--  , ss ~ SubState s
--  , se ~ SubElement s
--  ) => Iso m se
--one = Biparser (oneFw @c) bw
--  where
--  bw :: se -> n se
--  bw c = (tell =<< convertElement @c c) $> c

---- | Takes and writes substate. Updates the context and substate.
--split :: forall c s t m ss w.
--  ( SubStateContext c s
--  , MonadState s m
--  , ss ~ SubState s
--  )
--  => StateTransformer c ss m ss
--  -> Iso c t m s ss
--split = undefined
----split splitSubState = Biparser fw bw
----  where
----  fw = do
----    s <- get
----    (start, end) <- runStateT @c @ss splitSubState $ getSubState @s s
----    put $ updateSubStateContext @c s start end
----    return start
----  bw :: ss -> n ss
----  bw x = (tell =<< convertSequence @c x) $> x
----
------ | Takes and writes substate. Updates the context and substate.
----splitFw :: forall c s t m ss u.
----  ( MonadState s m
----  , SubStateContext c s
----  , SelectableStateT c
----  , ss ~ SubState s
----  )
----  => StateTransformer c ss m ss
----  -> Const c s t m u
----splitFw splitSubState = Biparser
----  do
----    s <- get
----    (start, end) <- runStateT @c @ss splitSubState $ getSubState @s s
----    put $ updateSubStateContext @c s start end
---- $ const $ pure ()

