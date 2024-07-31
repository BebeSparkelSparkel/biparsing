{-# LANGUAGE NoImplicitPrelude #-}
-- | Used to converte @u@ to the correct type for the biparser.
module Biparse.Comap (
comap,
comapM,
comapConst,
upon,
uponM,
uponConst,
) where

import Biparse.Core.Aliases (Biparser)
import Biparse.Core.Comap (comap, comapM)
import Data.Function (flip, (.), const)
import Data.Profunctor (Profunctor)
import Profunctor.Colift (Colift)

--comapMay :: forall p u u' v.
--  => (u -> Maybe u')
--  -> Biparser p u' v
--  -> Biparser p u  v
--comapMay f (Biparser fw bw) = Biparser fw $ bw <=< maybe (fail "backward map to Maybe gave Nothing.") pure . f
--
--comapEither :: forall p u u' v.
--  => (u -> Either v u')
--  -> Biparser p u' v
--  -> Biparser p u  v
--comapEither f (Biparser fw bw) = Biparser fw $ either pure bw . f
--
--comapPred :: forall p u v.
--  => (u -> Bool)
--  -> Biparser p u v
--  -> Biparser p u v
--comapPred p = comapM \u -> if p u then (fail "backward predicate failed.") else pure u
--
--comapPredM :: forall p u v.
--  => (u -> n Bool)
--  -> Biparser p u v
--  -> Biparser p u v
--comapPredM p = comapM \u -> bool (fail "backward monadic predicate failed") (pure u) =<< p u

comapConst :: forall p u u' v.
  Profunctor p
  => u'
  -> Biparser p u' v
  -> Biparser p u  v
comapConst = comap . const

infix 8 `upon`
upon :: forall p u u' v.
  Profunctor p
  => Biparser p u' v
  -> (u -> u')
  -> Biparser p u v
upon = flip comap

infix 8 `uponM`
uponM :: forall p m u u' v.
  Colift p m
  => Biparser p u' v
  -> (u -> m u')
  -> Biparser p u v
uponM = flip comapM

--infix 8 `uponMay`
--uponMay :: forall p u u' v.
--  => Biparser p u' v
--  -> (u -> Maybe u')
--  -> Biparser p u v
--uponMay = flip comapMay
--
--infix 8 `uponEither`
--uponEither :: forall p u u' v.
--  )
--  => Biparser p u' v
--  -> (u -> Either v u')
--  -> Biparser p u  v
--uponEither = flip comapEither
--
--infix 8 `uponPred`
--uponPred :: forall p u v.
--  => Biparser p u v
--  -> (u -> Bool)
--  -> Biparser p u v
--uponPred = flip comapPred
--
--infix 8 `uponPredM`
--uponPredM :: forall p u v.
--  => Biparser p u v
--  -> (u -> n Bool)
--  -> Biparser p u v
--uponPredM = flip comapPredM
--
infix 8 `uponConst`
uponConst :: forall p u u' v.
  Profunctor p
  => Biparser p u' v
  -> u'
  -> Biparser p u  v
uponConst = flip comapConst

