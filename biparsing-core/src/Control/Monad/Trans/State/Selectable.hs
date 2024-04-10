{-# LANGUAGE NoImplicitPrelude #-}
module Control.Monad.Trans.State.Selectable
  ( state
  , stateT
  , runState
  , runStateT
  , ContextualStateTransformerPLEASEREMOVESUFFIX
  , ContextualStateTransformer'
  , StateTransformer
  , SelectableStateT
  , SelectableStateTransformer(..)
  ) where

import Control.Applicative (Applicative(pure))
import Control.Monad.State.Class (MonadState)
import Control.Monad.State.Lazy qualified as L
import Control.Monad.State.Strict qualified as S
import Data.Function ((.))
import Data.Functor.Identity (Identity(runIdentity))
import Data.Kind (Type)

state :: forall context s m a.
  ( SelectableStateTransformer (StateTransformer context)
  , Applicative m
  ) => (s -> (a, s)) -> StateTransformer context s m a
state = stateTransformer' . (pure .)

stateT :: forall context s m a.
  ( SelectableStateTransformer (StateTransformer context)
  ) => (s -> m (a, s)) -> StateTransformer context s m a
stateT = stateTransformer'

runState :: forall context s a.
  ( SelectableStateTransformer (StateTransformer context)
  ) => StateTransformer context s Identity a -> s -> (a, s)
runState = (runIdentity .) . runStateTransformer'

runStateT :: forall context s m a.
  ( SelectableStateTransformer (StateTransformer context)
  ) => StateTransformer context s m a -> s -> m (a, s)
runStateT = runStateTransformer'

type ContextualStateTransformerPLEASEREMOVESUFFIX context s m = ContextualStateTransformer' context s m (StateTransformer context s m)
type ContextualStateTransformer' context s m m' =
  ( SelectableStateTransformer (StateTransformer context)
  , MonadState s m'
  , m' ~ StateTransformer context s m
  )

type StateTransformer :: Type -> Type -> (Type -> Type) -> Type -> Type
type family StateTransformer context

type SelectableStateT c = SelectableStateTransformer (StateTransformer c)

class SelectableStateTransformer t where
  stateTransformer' :: (s -> m (a, s)) -> t s m a
  runStateTransformer' :: forall s m a. t s m a -> s -> m (a, s)

instance SelectableStateTransformer L.StateT where
  stateTransformer' = L.StateT
  runStateTransformer' = L.runStateT

instance SelectableStateTransformer S.StateT where
  stateTransformer' = S.StateT
  runStateTransformer' = S.runStateT

