module Biparse.Core.Direction (
Direction(..),
WhichDirection,
) where

data Direction = Forward | Backward

type WhichDirection :: (Type -> Type) -> Direction
type family WhichDirection m
