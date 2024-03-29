{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}
module Biparse.Utils
  ( (!>)
  , (^:^)
  , (<$$>)
  , (<$$)
  , (>>>)

  , headAlt
  , lastAlt
  , tailAlt
  , initAlt
  , headTailAlt
  , ConvertIntegral(..)
  , ConvertSequence(..)
  , ConvertElement(..)
  , symbol
  , char
  , shouldBe
  ) where

import Control.Applicative (Applicative, pure, liftA2)
import Control.Monad (MonadFail, fail)
import Data.Eq (Eq((==)))
import Data.Function ((.), flip, ($))
import Data.Functor (Functor, (<$), fmap)
import Data.Functor.Alt (Alt, (<!>))
import Data.Maybe (maybe)
import Data.MonoTraversable (headMay, lastMay, MonoFoldable, Element, MonoPointed)
import Data.Proxy (Proxy(Proxy))
import Data.Sequences (IsSequence, initMay, tailMay, singleton)
import Data.String (IsString(fromString))
import GHC.Int (Int, Int64)
import GHC.Integer (Integer)
import GHC.Real (fromIntegral)
import GHC.TypeLits (KnownSymbol, symbolVal, KnownChar, charVal)
import Numeric.Natural (Natural)
import Text.Printf (IsChar(fromChar))

(!>) :: (Applicative f, Alt f) => f a -> a -> f a
x !> y = x <!> pure y

infixr 5 ^:^
(^:^) :: Applicative f => f a -> f [a] -> f [a]
(^:^) = liftA2 (:)

infixl 4 <$$>
(<$$>) :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
(<$$>) = fmap . fmap

infixl 4 <$$
(<$$) :: (Functor f, Functor g) => b -> f (g a) -> f (g b)
(<$$) = fmap . (<$)

infixr 9 >>>
(>>>) :: (a -> b) -> (b -> c) -> a -> c
(>>>) = flip (.)

headAlt :: forall a n. (MonadFail n, MonoFoldable a) => a -> n (Element a)
headAlt = maybe (fail "Could not take the head of the collection.") pure . headMay

lastAlt :: forall a n. (MonadFail n, MonoFoldable a) => a -> n (Element a)
lastAlt = maybe (fail "Could not take the last element of the collection.") pure . lastMay

tailAlt :: forall a n. (MonadFail n, IsSequence a) => a -> n a
tailAlt = maybe (fail "Could not take the tail of the collection.") pure . tailMay

initAlt :: forall a n. (MonadFail n, IsSequence a) => a -> n a
initAlt = maybe (fail "Could not take the inital elements of the collection.") pure . initMay

headTailAlt :: (IsSequence b, MonadFail m) => b -> m (Element b, b)
headTailAlt x = liftA2 (,) (headAlt x) (tailAlt x)

class ConvertIntegral a b where convertIntegral :: a -> b
instance ConvertIntegral Natural Int where convertIntegral = fromIntegral
instance ConvertIntegral Natural Int64 where convertIntegral = fromIntegral
instance ConvertIntegral Natural Integer where convertIntegral = fromIntegral

class ConvertSequence context a b m where convertSequence :: a -> m b
instance Applicative m => ConvertSequence () a a m where convertSequence = pure

class ConvertElement context a b m where convertElement :: a -> m b
instance (e ~ Element seq, MonoPointed seq, Applicative m) => ConvertElement () e seq m where convertElement = pure . singleton

symbol :: forall s a. (KnownSymbol s, IsString a) => a
symbol = fromString $ symbolVal $ Proxy @s

char :: forall c a. (KnownChar c, IsChar a) => a
char = fromChar $ charVal $ Proxy @c

shouldBe :: (Eq a, MonadFail m) => a -> a -> m a
shouldBe x y = if x == y then pure x else (fail "x should be y but it is not.")

