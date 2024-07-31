{-# LANGUAGE NoImplicitPrelude #-}
module Profunctor.Colift (Colift(..)) where

import Data.Kind (Type, Constraint)

type Colift :: (Type -> Type -> Type) -> (Type -> Type) -> Constraint
class Colift p m | p -> m where colift :: (a -> m b) -> p b c -> p a c
