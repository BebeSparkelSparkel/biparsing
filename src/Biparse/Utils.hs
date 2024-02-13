{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}
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
  , ConvertString'(..)
  , ConvertString
  , convertString
  , ConvertStringInstanceSelector
  , ConvertStringSelector
  , IdentityInstance
  , FromStringInstance
  , ToStringInstance
  , symbol
  , char
  , shouldBe
  ) where

import Control.Applicative (Applicative, pure, liftA2)
import Control.Monad (MonadFail, fail)
import Data.Char (Char)
import Data.Eq (Eq((==)))
import Data.Function ((.), flip, ($), id)
import Data.Functor (Functor, (<$), fmap)
import Data.Functor.Alt (Alt, (<!>))
import Data.Kind (Type)
import Data.Maybe (maybe)
import Data.MonoTraversable (headMay, lastMay, MonoFoldable, Element)
import Data.Proxy (Proxy(Proxy))
import Data.Sequences (IsSequence, initMay, tailMay)
import Data.String (String, IsString(fromString))
import GHC.Exts (IsList, toList, Item)
import GHC.Int (Int, Int64)
import GHC.Integer (Integer)
import GHC.Real (fromIntegral)
import GHC.TypeLits (KnownSymbol, symbolVal, KnownChar, charVal)
import Numeric.Natural (Natural)
import Text.Printf (IsChar(fromChar))

(|>) :: (Applicative f, Alt f) => f a -> a -> f a
x |> y = x <!> pure y

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

type ConvertStringInstanceSelector :: Type -> Type -> Type
type family ConvertStringInstanceSelector a b where
  ConvertStringInstanceSelector a a = IdentityInstance
  ConvertStringInstanceSelector String _ = FromStringInstance
  ConvertStringInstanceSelector _ String = ToStringInstance
  ConvertStringInstanceSelector a b = ConvertStringSelector a b
type ConvertStringSelector :: Type -> Type -> Type
type family ConvertStringSelector a b

type ConvertString a b = ConvertString' (ConvertStringInstanceSelector a b) a b
convertString :: forall a b. ConvertString a b => a -> b
convertString = convertString' @(ConvertStringInstanceSelector a b)
class ConvertString' is a b where convertString' :: a -> b
data IdentityInstance
instance ConvertString' IdentityInstance a a where convertString' = id
data FromStringInstance
instance IsString b => ConvertString' FromStringInstance String b where convertString' = fromString
data ToStringInstance
instance {-# OVERLAPPABLE #-} (IsList a, Item a ~ Char) => ConvertString' ToStringInstance a String where convertString' = toList

symbol :: forall s a. (KnownSymbol s, IsString a) => a
symbol = fromString $ symbolVal $ Proxy @s

char :: forall c a. (KnownChar c, IsChar a) => a
char = fromChar $ charVal $ Proxy @c

shouldBe :: (Eq a, MonadFail m) => a -> a -> m a
shouldBe x y = if x == y then pure x else (fail "x should be y but it is not.")

