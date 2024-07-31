{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module Control.Monad.ChangeMonad
  ( ChangeMonad(..)
  --, changeMonad
  --, ChangeFunction
  --, ResultMonad(..)
  , Lift
  ) where

--import System.IO (IO)

-- * ChangeMonad

class ChangeMonad instanceSelector m n changeArg | instanceSelector m n -> changeArg where
  changeMonad' :: changeArg -> m a -> n a

--type ChangeFunction :: Type -> (Type -> Type) -> (Type -> Type) -> Type
--type family ChangeFunction instanceSelector m n

--changeMonad :: forall m n.
--  ( ChangeMonad () m n ()
--  --, ChangeFunction () m n ~ ()
--  ) => forall a. m a -> n a
--changeMonad = changeMonad' @() ()
--
--instance ChangeMonad () m m () where
--  changeMonad' = const id

--type instance ChangeFunction () m m = ()

---- * ResultMonad
--
--class ResultMonad (m :: Type -> Type) instanceSelector where
--  type ResultingMonad (m :: Type -> Type) instanceSelector :: Type -> Type
--  resultMonad :: ChangeFunction instanceSelector m (ResultingMonad m instanceSelector)
--
--instance ResultMonad Identity () where
--  type ResultingMonad Identity () = Identity
--  resultMonad = ()
--
--instance ResultMonad IO  () where
--  type ResultingMonad IO () = IO
--  resultMonad = ()
--
--instance ResultMonad Maybe () where
--  type ResultingMonad Maybe () = Maybe
--  resultMonad = ()
--
--instance ResultMonad EitherString () where
--  type ResultingMonad EitherString () = EitherString
--  resultMonad = ()

-- | Used for the 'MonadTrans' 'lift' function for changing the monad.
data Lift

