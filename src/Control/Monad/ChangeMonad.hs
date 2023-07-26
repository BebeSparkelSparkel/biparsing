{-# LANGUAGE AllowAmbiguousTypes #-}
module Control.Monad.ChangeMonad
  ( ChangeMonad(..)
  , ResultMonad(..)
  ) where

import System.IO (IO)

class ChangeMonad instanceSelector m n where
  type ChangeFunction instanceSelector m n
  changeMonad :: ChangeFunction instanceSelector m n -> m a -> n a

--instance ChangeMonad is Identity Identity where
--  type ChangeFunction _ Identity Identity = ()
--  changeMonad = const id
--
--instance ChangeMonad is IO IO where
--  type ChangeFunction _ IO IO = ()
--  changeMonad = const id
--
--instance ChangeMonad is Maybe Maybe where
--  type ChangeFunction _ Maybe Maybe = ()
--  changeMonad = const id

instance ChangeMonad () m m where
  type ChangeFunction _ _ _ = ()
  changeMonad = const id

class ResultMonad (m :: Type -> Type) instanceSelector where
  type ResultingMonad (m :: Type -> Type) instanceSelector :: Type -> Type
  resultMonad :: ChangeFunction instanceSelector m (ResultingMonad m instanceSelector)

instance ResultMonad Identity () where
  type ResultingMonad Identity () = Identity
  resultMonad = ()

instance ResultMonad IO  () where
  type ResultingMonad IO () = IO
  resultMonad = ()

instance ResultMonad Maybe () where
  type ResultingMonad Maybe () = Maybe
  resultMonad = ()

