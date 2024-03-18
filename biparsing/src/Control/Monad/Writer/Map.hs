module Control.Monad.Writer.Map
  ( MapWriter(..)
  ) where

type MapWriter :: (Type -> Type) -> Constraint
class MapWriter m where
  type WriteType m :: Type
  type ChangeWriteType m w :: Type -> Type
  mapWriter :: Monoid w => (w -> WriteType m) -> ChangeWriteType m w a -> m a

