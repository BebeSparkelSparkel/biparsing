{-# LANGUAGE AllowAmbiguousTypes #-}
module Control.Monad.ChangeMonad
  ( ChangeMonad(..)
  , ResultMonad(..)
  ) where

import System.IO (IO)

class ChangeMonad m n where
  type ChangeFunction m n
  changeMonad :: ChangeFunction m n -> m a -> n a

--instance ChangeMonad m m where
--  type ChangeFunction m m = ()
--  changeMonad = const id

instance ChangeMonad Identity Identity where
  type ChangeFunction Identity Identity = ()
  changeMonad = const id

instance ChangeMonad IO IO where
  type ChangeFunction IO IO = ()
  changeMonad = const id

instance ChangeMonad Maybe Maybe where
  type ChangeFunction Maybe Maybe = ()
  changeMonad = const id

--instance ChangeMonad (Either a) (Either a) where
--  type ChangeFunction (Either a) (Either a) = ()
--  changeMonad = const id

class ResultMonad (m :: Type -> Type) where
  type ResultingMonad (m :: Type -> Type) :: Type -> Type
  resultMonad :: ChangeFunction m (ResultingMonad m)

instance ResultMonad Identity where
  type ResultingMonad Identity = Identity
  resultMonad = ()

instance ResultMonad IO where
  type ResultingMonad IO = IO
  resultMonad = ()

instance ResultMonad Maybe where
  type ResultingMonad Maybe = Maybe
  resultMonad = ()

