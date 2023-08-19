module Control.Monad.TransformerBaseMonad
  ( TransformerBaseMonad
  , LiftBaseMonad(..)
  ) where

import Control.Monad.Trans (lift)

type TransformerBaseMonad :: (Type -> Type) -> (Type -> Type)
type family TransformerBaseMonad a

type instance TransformerBaseMonad (WriterT _ m) = m

class LiftBaseMonad m where liftBaseMonad :: TransformerBaseMonad m a -> m a

instance (Monoid w, Monad m) => LiftBaseMonad (WriterT w m) where liftBaseMonad = lift

