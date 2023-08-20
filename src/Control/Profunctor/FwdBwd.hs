{-# LANGUAGE AllowAmbiguousTypes #-}
module Control.Profunctor.FwdBwd
  ( (:*:)(..)
  , Fwd(..)
  , Bwd(..)
  , BwdMonad
  , Comap(..)
  ) where

import GHC.Generics (Generic, Generic1)
import Generic.Data (gpure, gap, gempty, galt)

data (:*:) p q u v = (:*:) {pfst :: p u v, psnd :: q u v} deriving (Functor, Generic, Generic1)
instance (Applicative (p u), Applicative (q u)) => Applicative ((:*:) p q u) where
  pure = gpure
  (<*>) = gap
instance (Alternative (p u), Alternative (q u)) => Alternative ((:*:) p q u) where
  empty = gempty
  (<|>) = galt
instance (Monad (p u), Monad (q u)) => Monad ((:*:) p q u) where
  (fw :*: bw) >>= f = (fw >>= pfst . f) :*: (bw >>= psnd . f)
instance (MonadFail (p u), MonadFail (q u)) => MonadFail ((:*:) p q u) where
  fail msg = fail msg :*: fail msg

newtype Fwd m u v = Fwd {unFwd :: m v} deriving (Functor, Applicative, Alternative, Monad, MonadFail)

newtype Bwd m u v = Bwd {unBwd :: u -> m v} deriving (Functor, Generic1)
instance Applicative m => Applicative (Bwd m u) where
  pure = gpure
  (<*>) = gap
instance Monad m => Monad (Bwd m u) where
  Bwd bw >>= f = Bwd \u -> bw u >>= ($ u) . unBwd . f
instance Alternative m => Alternative (Bwd m u) where
  empty = Bwd $ const empty
  Bwd x <|> Bwd y = Bwd \u -> x u <|> y u
instance MonadFail m => MonadFail (Bwd m u) where
  fail = Bwd . const . fail

type BwdMonad :: Type -> (Type -> Type -> Type) -> Type -> Type
type family BwdMonad instanceSelector m
infix 8 `upon`, `uponM`
class Comap is (p :: Type -> Type -> Type) where
  comap :: (u -> u') -> p u' v -> p u v
  comapM :: (u -> BwdMonad is p u') -> p u' v -> p u v
  upon :: p u' v -> (u -> u') -> p u v
  upon = flip $ comap @is
  uponM :: p u' v -> (u -> BwdMonad is p u') -> p u v
  uponM = flip $ comapM @is

type instance BwdMonad () (_ :*: Bwd n) = n
instance Monad n => Comap () (Fwd m :*: Bwd n) where
  comap f (Fwd fw :*: Bwd bw) = Fwd fw :*: Bwd (bw . f)
  comapM f (Fwd fw :*: Bwd bw) = Fwd fw :*: Bwd (bw <=< f)

