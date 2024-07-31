-- | Used to converte @u@ to the correct type for the biparser.
module Biparse.Core.Comap (
comap,
comapM,
) where

import Biparse.Core.Aliases (Biparser)

comap :: forall p u u' v.
  Profunctor p
  => (u -> u')
  -> Biparser p u' v
  -> Biparser p u v
comap = lmap

comapM :: forall p m u u' v.
  Colift p m
  => (u -> m u')
  -> Biparser p u' v
  -> Biparser p u v
comapM = colift

