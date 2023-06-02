{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Biparse.AlternativeAttributes
  ( AlternativeAttributes
  , AA
  , (<<|>>)
  , runAtt
  , totalAtt
  , addAtt
  ) where

import Data.Bool (Bool(True,False))
import Data.Type.Bool (If)
import Control.Applicative (Alternative((<|>)))
import Data.Function (($))

-- * Type Level Alternative Attributes
-- Ensures that all tokens have an implementation. Does not ensure that all can be produced.

type AlternativeAttributes as a = AA as a

newtype AA (attributes :: [k]) a = AA a

runAtt :: forall required implemented a.
  ( HasAll required implemented
  ) => AA implemented a -> a
runAtt (AA x) = x

totalAtt :: forall required implemented a b last total.
  ( total ~ last : implemented
  )
  => AA implemented a
  -> AA '[last] (a -> b)
  -> AA total b
totalAtt (AA x) (AA f) = AA $ f x

addAtt :: forall k a. a -> AA '[k] a
addAtt = AA

(<<|>>) :: forall as as' f a. Alternative f => AA as (f a) -> AA as' (f a) -> AA (as ++ as') (f a)
AA x <<|>> AA y = AA $ x <|> y

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

type (++) :: [a] -> [a] -> [a]
type family xs ++ ys where
  (x : xs) ++ ys = x : (xs ++ ys)
  '[] ++ ys = ys


