{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE StandaloneKindSignatures #-}
module Biparse.Control.Fwd (Fwd(..)) where

import Control.Monad.State.Class (MonadState(get,put))
import Debug.Trace (trace)
import Data.Functor.Const
import Data.Functor.Compose
import Data.Bifunctor.Joker (Joker(Joker))

type Flip :: (a -> b -> Type) -> b -> a -> Type
newtype Flip p b a = Flip (p a b)

-- | 'u' is thrown away
newtype Fwd m u a = Fwd {runFwd :: m a}
  deriving (Show, Eq, Functor, Applicative, Alternative, Monad, MonadFail, Peek, Try)
  deriving (MonadState s, MonadError e) via m
  deriving Profunctor via Joker m

type instance Item' (Fwd m) = Item' m
instance OneFwd a m => One a (Fwd m) where one = Fwd oneFwd

type instance WhichDirection (Fwd _ _) = 'Forward
