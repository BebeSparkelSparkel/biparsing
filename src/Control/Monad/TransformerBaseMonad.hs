module Control.Monad.TransformerBaseMonad
  ( TransformerBaseMonad
  , LiftBaseMonad(..)
  ) where

type TransformerBaseMonad :: (Type -> Type) -> (Type -> Type)
type family TransformerBaseMonad a


class LiftBaseMonad m where liftBaseMonad :: TransformerBaseMonad m a -> m a

type instance TransformerBaseMonad (WriterT _ m) = m
instance (Monoid w, Monad m) => LiftBaseMonad (WriterT w m) where liftBaseMonad = lift

type instance TransformerBaseMonad (RWST _ _ _ m) = m
instance (Monoid w, Monad m) => LiftBaseMonad (RWST r w s m) where liftBaseMonad = lift

