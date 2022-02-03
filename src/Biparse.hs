module Biparse where

import GHC.Generics
import Data.Bifunctor (Bifunctor)
import Generic.Functor ( GenericBifunctor(..) )

data Biparser a b = Biparser { parse :: a, serialize :: b }
  deriving Generic
  deriving Bifunctor via (GenericBifunctor Biparser)

type Iso a b = Biparser (a -> b) (b -> a)
type Questionable e a b = Biparser (a -> Either e b) (b -> a)

adtConstructor :: m a -> (a -> b) -> Biparser (m a) (a -> b)
adtConstructor = Biparser

-- (<>) :: Biparser a b c d -> Biparser b e d f -> Biparser a e c f
-- Biparser ab cd <> Biparser be df = Biparser (be . ab) (df . cd)
