-- | Used to converte @u@ to the correct type for the biparser.
module Biparse.Core.Comap (
comap,
comapM,
Profunctor,
ComapM,
) where

import Data.Profunctor qualified
import Profunctor.Monad.Cofunctor qualified
import Profunctor.Monad.Cofunctor (First)
import Control.Arrow (Kleisli)

comap :: forall p u u' v.
  Profunctor p
  => (u -> u')
  -> p u' v
  -> p u v
comap = Data.Profunctor.lmap

type ComapM p m =
  ( Cofunctor p
  , First p ~ Kleisli m
  )

comapM :: forall p m u u' v.
  ComapM p m
  => (u -> m u')
  -> p u' v
  -> p u v
comapM = (Profunctor.Monad.Cofunctor.=:)

