{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE InstanceSigs #-}
module Biparse.Unordered
  ( unordered
  , Unordered(..)
  , WrapUnwrap(..)
  , wrappedOne
  ) where

import Biparse.Biparser
import GHC.Generics
import Data.Functor.Compose
import Control.Lens (Lens, lens, (%~), (^.))
import Control.Monad.Trans.Error (Error)
import Data.Proxy (Proxy)

import GHC.Err (undefined)

data WrapUnwrap a b = WrapUnwrap (forall m. Alternative m => a -> m b) (forall n. Applicative n => b -> n a)

wrappedOne :: forall c m n s a b ss.
  ( Element ss ~ a
  , MonadPlus m
  , Alternative n
  , One c s m n ss
  )
  => WrapUnwrap a b -> Iso c m n s b
wrappedOne (WrapUnwrap f g) = do
  x <- one `uponM` g
  f x

unordered :: forall c m n s a.
  ( Generic a
  , Monad m
  , Monad n
  , Unordered c m n s (Rep a)
  ) => Iso c m n s a
unordered = to <$> (unordered' :: Iso c m n s (Rep a x)) `upon` from

class Unordered c m n s f where unordered' :: Iso c m n s (f a)

instance (Unordered c m n s sel, Functor m, Monad n)  => Unordered c m n s (D1 i (C1 i' sel)) where
  unordered' = m1 . m1 $ unordered' @c @m @n @s @sel

instance Unordered c m n s (a :*: b) where
  unordered' = _

m1 :: (Functor m, Monad n) => Iso c m n s (f a) -> Iso c m n s (M1 i c' f a)
m1 = comap unM1 . fmap M1

