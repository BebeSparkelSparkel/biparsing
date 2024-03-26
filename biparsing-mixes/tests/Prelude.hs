{-# LANGUAGE NoImplicitPrelude #-}
module Prelude
  ( module Biparse.Mixes.SubStates
  , module Control.Monad
  , module Control.Monad.ChangeMonad
  , module Control.Monad.EitherString
  , module Control.Monad.Trans.RWS.CPS
  , module Data.Default
  , module Data.Eq
  , module Data.Foldable
  , module Data.Function
  , module Data.Functor
  , module Data.Ix
  , module Data.Monoid
  , module Data.Sequences
  , module Data.String
  , module Data.Word
  , module GHC.Err
  , module Test.Hspec
  , module Text.Printf
  , module Text.Show

  , contexts
  , subStates
  , combinations
  ) where

import Biparse.Mixes.SubStates
import Control.Monad ((>>=))
import Control.Monad.ChangeMonad (ChangeMonad)
import Control.Monad.EitherString (EitherString)
import Control.Monad.Trans.RWS.CPS (RWST)
import Data.ByteString.Builder qualified as BB
import Data.ByteString.Internal (c2w, w2c)
import Data.Default (Default, def)
import Data.Eq (Eq, (==))
import Data.Foldable (foldMap)
import Data.Function
import Data.Functor ((<$>))
import Data.Ix (Ix)
import Data.Monoid (Monoid)
import Data.Sequences (IsSequence)
import Data.String (String, IsString, fromString)
import Data.Word (Word8)
import GHC.Err (undefined)
import Language.Haskell.TH (Name, mkName, TypeQ, conT)
import Test.Hspec
import Text.Printf (IsChar, fromChar, toChar)
import Text.Show (Show)

instance IsString Name where fromString = mkName
instance IsString TypeQ where fromString = conT . mkName

instance IsChar Word8 where
  fromChar = c2w
  toChar = w2c

contexts :: [TypeQ]
contexts =
  [ "IndexContext"
  , "UnixLC"
  , "WindowsLC"
  , "LinesOnly"
  , "ColumnsOnly"
  , "LineColumnUnknownBreak"
  , "NoUpdate"
  ]

subStates :: [TypeQ]
subStates =
  [ "String"
  , "StrictByteString"
  , "LazyByteString"
  , "StrictText"
  , "LazyText"
  ]

combinations :: [a] -> [b] -> [(a,b)]
combinations xs ys = foldMap (\x -> (x,) <$> ys) xs

instance Eq BB.Builder where x == y = BB.toLazyByteString x == BB.toLazyByteString y

