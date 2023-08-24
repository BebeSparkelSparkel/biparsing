{-# LANGUAGE NoImplicitPrelude #-}
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
  ) where

import Data.Sequences (IsSequence, initMay, tailMay)
import Control.Applicative (Applicative, Alternative((<|>)), pure, empty, liftA2)
import Data.Functor (Functor, (<$), fmap)
import Data.MonoTraversable (headMay, lastMay, MonoFoldable, Element)
import Data.Maybe (maybe)
import Data.Function ((.), flip)

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

