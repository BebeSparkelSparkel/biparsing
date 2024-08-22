{-# LANGUAGE NoImplicitPrelude #-}
-- | Used to converte @u@ to the correct type for the biparser.
module Biparse.Comap (
module Biparse.Core.Comap,
comapConst,
unit,
upon,
uponM,
uponConst,
) where

import Biparse.Core.Aliases (Unit, Const)
import Biparse.Core.Comap (comap, comapM, Profunctor, ComapM)
import Data.Function (flip, (.), const, ($))

--comapMay :: forall p u u' v.
--  => (u -> Maybe u')
--  -> p u' v
--  -> p u  v
--comapMay f (fw bw) = fw $ bw <=< maybe (fail "backward map to Maybe gave Nothing.") pure . f
--
--comapEither :: forall p u u' v.
--  => (u -> Either v u')
--  -> p u' v
--  -> p u  v
--comapEither f (fw bw) = fw $ either pure bw . f
--
--comapPred :: forall p u v.
--  => (u -> Bool)
--  -> p u v
--  -> p u v
--comapPred p = comapM \u -> if p u then (fail "backward predicate failed.") else pure u
--
--comapPredM :: forall p u v.
--  => (u -> n Bool)
--  -> p u v
--  -> p u v
--comapPredM p = comapM \u -> bool (fail "backward monadic predicate failed") (pure u) =<< p u

comapConst :: forall p u u' v.
  Profunctor p
  => u'
  -> p u' v
  -> p u  v
comapConst = comap . const

-- | Throws away @u@ and @v@
unit :: forall p u. Profunctor p => Unit p -> Const p u
unit = comap $ const ()

infix 8 `upon`
upon :: forall p u u' v.
  Profunctor p
  => p u' v
  -> (u -> u')
  -> p u v
upon = flip comap

infix 8 `uponM`
uponM :: forall p m u u' v.
  ComapM p m
  => p u' v
  -> (u -> m u')
  -> p u v
uponM = flip comapM

--infix 8 `uponMay`
--uponMay :: forall p u u' v.
--  => p u' v
--  -> (u -> Maybe u')
--  -> p u v
--uponMay = flip comapMay
--
--infix 8 `uponEither`
--uponEither :: forall p u u' v.
--  )
--  => p u' v
--  -> (u -> Either v u')
--  -> p u  v
--uponEither = flip comapEither
--
--infix 8 `uponPred`
--uponPred :: forall p u v.
--  => p u v
--  -> (u -> Bool)
--  -> p u v
--uponPred = flip comapPred
--
--infix 8 `uponPredM`
--uponPredM :: forall p u v.
--  => p u v
--  -> (u -> n Bool)
--  -> p u v
--uponPredM = flip comapPredM
--
infix 8 `uponConst`
uponConst :: forall p u u' v.
  Profunctor p
  => p u' v
  -> u'
  -> p u  v
uponConst = flip comapConst

