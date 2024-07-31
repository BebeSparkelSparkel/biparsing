{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE UndecidableInstances #-}
module Biparse.Utils
  ( (!>)
  , (<$$>)
  , (<$$)
  , (>>>)

  , headAlt
  , lastAlt
  , tailAlt
  , initAlt
  , headTailAlt
  , symbol
  , char
  ) where

import Control.Applicative (Applicative, pure, liftA2)
import Control.Monad (MonadFail, fail)
import Data.Function ((.), flip, ($))
import Data.Functor (Functor, (<$), fmap)
import Data.Functor.Alt (Alt, (<!>))
import Data.Maybe (maybe)
import Data.MonoTraversable (headMay, lastMay, MonoFoldable, Element)
import Data.Proxy (Proxy(Proxy))
import Data.Sequences (IsSequence, initMay, tailMay)
import Data.String (IsString(fromString))
import GHC.TypeLits (KnownSymbol, symbolVal, KnownChar, charVal)
import Text.Printf (IsChar(fromChar))

(!>) :: (Applicative f, Alt f) => f a -> a -> f a
x !> y = x <!> pure y

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

symbol :: forall s a. (KnownSymbol s, IsString a) => a
symbol = fromString $ symbolVal $ Proxy @s

char :: forall c a. (KnownChar c, IsChar a) => a
char = fromChar $ charVal $ Proxy @c

