module Biparse.Context.IdentityState
  ( IdentityState
  ) where

import Biparse.Biparser (SubState, GetSubState(getSubState), UpdateStateWithSubState(updateSubStateContext), UpdateStateWithElement(updateElementContext))

-- * Identity Context
-- Use as the context if @state ~ SubState IdentityState state@ basically if there is no context outside the 

data IdentityState
type instance SubState IdentityState a = a
instance GetSubState IdentityState state where
  getSubState = id

instance UpdateStateWithSubState IdentityState state where
  updateSubStateContext _ _ s = s

instance UpdateStateWithElement IdentityState state where
  updateElementContext _ _ s = s

