{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Biparse.AlternativeAttributes
  ( AlternativeAttributes
  , AA
  , A
  , a
  , (<|>>)
  --, (<<|>>)
  , runAtt
  , totalAtt
  , emptyAtt
  --, addAtt
  ) where

import Data.Bool (Bool(True,False))
import Data.Type.Bool (If)
import Control.Applicative (Alternative((<|>),empty))
import Data.Function (($))

-- * Type Level Alternative Attributes
-- Ensures that all tokens have an implementation. Does not ensure that all can be produced.

type AlternativeAttributes as a = AA as a

newtype AA (attributes :: [k]) a = AA a

newtype A (attribute :: k) a = A a

a :: forall {k} (attribute :: k) a. a -> A attribute a
a = A

runAtt :: forall required implemented a.
  ( HasAll required implemented
  ) => AA implemented a -> a
runAtt (AA x) = x

totalAtt :: forall implemented a b last total.
  ( total ~ last : implemented
  )
  => AA implemented a
  -> A last (a -> b)
  -> AA total b
totalAtt (AA x) (A f) = AA $ f x

emptyAtt :: Alternative f => AA '[] (f a)
emptyAtt = AA empty

--addAtt :: forall k a. a -> AA '[k] a
--addAtt = AA

infixr 9 <|>>
(<|>>) :: forall {k} (a :: k) (as :: [k]) f b. Alternative f => A a (f b) -> AA as (f b) -> AA (a : as) (f b)
A x <|>> AA y = AA $ x <|> y

--(<<|>>) :: forall as as' f a. Alternative f => AA as (f a) -> AA as' (f a) -> AA (as ++ as') (f a)
--AA x <<|>> AA y = AA $ x <|> y

type HasAll required implemented = Complement required implemented ~ '[]

type Complement :: [a] -> [a] -> [a]
type family Complement xs ys where
  Complement (x : xs) ys = If (HasElement x ys) (Complement xs ys) (x : Complement xs ys)
  Complement '[] _ = '[]

type HasElement :: a -> [a] -> Bool
type family HasElement x ys where
  HasElement x (x : _) = 'True
  HasElement x (_ : ys) = HasElement x ys
  HasElement _ '[] = 'False

