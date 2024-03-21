module Biparse.AssociatedWriter (AssociatedWriter) where

import Data.Kind (Type)

type AssociatedWriter :: Type -> Type
type family AssociatedWriter ss

type instance AssociatedWriter [a] = [a] -- -> [a]
type instance AssociatedWriter StrictByteString = BuilderByteString
type instance AssociatedWriter LazyByteString = BuilderByteString
type instance AssociatedWriter StrictText = BuilderText
type instance AssociatedWriter LazyText = BuilderText

