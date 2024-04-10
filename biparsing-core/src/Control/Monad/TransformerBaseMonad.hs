module Control.Monad.TransformerBaseMonad {-# DEPRECATED "Use ChangeMonad" #-}
  ( TransformerBaseMonad
  , LiftBaseMonad(..)
  ) where

import Control.Monad.RWS qualified
import Control.Monad.RWS.CPS qualified
import Control.Monad.Writer (WriterT)

type TransformerBaseMonad :: (Type -> Type) -> (Type -> Type)
type family TransformerBaseMonad a

class LiftBaseMonad m where liftBaseMonad :: TransformerBaseMonad m a -> m a

type instance TransformerBaseMonad (WriterT _ m) = m
instance (Monoid w, Monad m) => LiftBaseMonad (WriterT w m) where liftBaseMonad = lift

type instance TransformerBaseMonad (Control.Monad.RWS.RWST _ _ _ m) = m
instance (Monoid w, Monad m) => LiftBaseMonad (Control.Monad.RWS.RWST r w s m) where liftBaseMonad = lift

type instance TransformerBaseMonad (Control.Monad.RWS.CPS.RWST _ _ _ m) = m
instance Monad m => LiftBaseMonad (Control.Monad.RWS.CPS.RWST r w s m) where liftBaseMonad = lift

