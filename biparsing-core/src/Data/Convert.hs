{-# LANGUAGE NoImplicitPrelude #-}
module Data.Convert
  ( ConvertElement(convertElement)
  , ConvertSequence(convertSequence)
  ) where

import Control.Applicative (Applicative, pure)
import Data.Function ((.))
import Data.MonoTraversable (Element, MonoPointed(opoint))

class ConvertElement context a b m where convertElement :: a -> m b
instance (e ~ Element seq, MonoPointed seq, Applicative m) => ConvertElement () e seq m where convertElement = pure . opoint

class ConvertSequence context a b m where convertSequence :: a -> m b
instance Applicative m => ConvertSequence () a a m where convertSequence = pure
