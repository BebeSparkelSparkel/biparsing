{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE UndecidableInstances #-}
module Biparse.Core.Classes.Bidirectional (
Item',
One(one),
--BiN(biN),
StripPrefix(..),
Diverge(..),
) where

--import Biparse.Core.Alternative (Alternative)

type Item' :: k -> Type
type family Item' a

-- | Returns one element.
class One a m | m -> a where one :: m a
deriving instance One a m => One a (IdentityT m)

-- | Returns n elements.
-- When writing (backwards) all characters should probably be writtern not just N.
--class BiN a m | m -> a where biN :: Int -> m a

class StripPrefix a m | m -> a where stripPrefix :: a -> m ()
--stripPrefix :: forall p seq u eq.
--  ( Profunctor p
--  , BiN seq (p seq)
--  , Try (p u)
--  , MonadFail (p u)
--  , Length seq
--  , Show seq
--  , Applicative (EqualityWrapper (StripPrefixEqualityCheck p))
--  , eq ~ EqualityWrapper (StripPrefixEqualityCheck p) seq
--  , Eq eq
--  )
--  => seq
--  -> Const p u
--stripPrefix prefix = try do
--  xs <- biN (length prefix) `uponConst` prefix
--  unless ((pure prefix :: eq) == pure xs) $ fail $ "Could not match prefix: " <> show prefix

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

