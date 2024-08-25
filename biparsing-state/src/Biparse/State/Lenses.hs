{-# LANGUAGE FunctionalDependencies #-}
module Biparse.State.Lenses (
HasDataId(..),
lens,
makeLensesFor,
) where

import Lens.Micro (Lens)

class HasDataId a s t b | s -> a, t -> b, s b -> t, t a -> s where dataId :: Lens s t a b

