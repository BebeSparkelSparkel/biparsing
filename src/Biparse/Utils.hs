{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module Biparse.Utils
  ( (|>)
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
  , convertIntegralUnsafe
  , symbol
  , char
  , shouldBe
  ) where

import Control.Applicative (Applicative, Alternative((<|>)), pure, empty, liftA2)
import Data.Function ((.), flip, ($))
import Data.Functor (Functor, (<$), fmap)
import Data.Maybe (maybe)
import Data.MonoTraversable (headMay, lastMay, MonoFoldable, Element)
import Data.Sequences (IsSequence, initMay, tailMay)
import GHC.Num (Num, fromInteger)
import GHC.Real (Integral, toInteger)
import GHC.Integer (Integer)
import GHC.Int (Int)
import Numeric.Natural (Natural)
import GHC.TypeLits (KnownSymbol, symbolVal, KnownChar, charVal)
import Data.String (IsString(fromString))
import Data.Proxy (Proxy(Proxy))
import Data.Eq (Eq((==)))
import Text.Printf (IsChar(fromChar))

(|>) :: Alternative f => f a -> a -> f a
x |> y = x <|> pure y

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

headAlt :: forall a n. (Alternative n, MonoFoldable a) => a -> n (Element a)
headAlt = maybe empty pure . headMay

lastAlt :: forall a n. (Alternative n, MonoFoldable a) => a -> n (Element a)
lastAlt = maybe empty pure . lastMay

tailAlt :: forall a n. (Alternative n, IsSequence a) => a -> n a
tailAlt = maybe empty pure . tailMay

initAlt :: forall a n. (Alternative n, IsSequence a) => a -> n a
initAlt = maybe empty pure . initMay

headTailAlt :: (IsSequence b, Alternative m) => b -> m (Element b, b)
headTailAlt x = liftA2 (,) (headAlt x) (tailAlt x)

class ConvertIntegral a b where convertIntegral :: a -> b
-- | Unsafe because not number types are compatable for confersion. Use do define appropriate instances of ConvertIntegral
convertIntegralUnsafe :: forall a b. (Integral a, Num b) => a -> b
convertIntegralUnsafe = fromInteger . toInteger
instance ConvertIntegral Natural Int where convertIntegral = convertIntegralUnsafe
instance ConvertIntegral Natural Integer where convertIntegral = convertIntegralUnsafe

symbol :: forall s a. (KnownSymbol s, IsString a) => a
symbol = fromString $ symbolVal $ Proxy @s

char :: forall c a. (KnownChar c, IsChar a) => a
char = fromChar $ charVal $ Proxy @c

shouldBe :: (Eq a, Alternative m) => a -> a -> m a
shouldBe x y = if x == y then pure x else empty

