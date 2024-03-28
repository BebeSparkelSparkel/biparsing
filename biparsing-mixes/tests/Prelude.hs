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
  , module Data.Either
  , module System.IO

  , monads
  , contexts
  , subStates
  , shouldReturn
  ) where

import Biparse.Mixes.SubStates
import Control.Monad
import Control.Monad.ChangeMonad (ChangeMonad)
import Control.Monad.EitherString (EitherString)
import Control.Monad.Trans.RWS.CPS (RWST)
import Data.ByteString.Builder qualified as BB
import Data.ByteString.Internal (c2w, w2c)
import Data.Default (Default, def)
import Data.Eq (Eq, (==))
import Data.Foldable (foldMap)
import Data.Function
import Data.Functor
import Data.Ix (Ix)
import Data.Monoid (Monoid, (<>))
import Data.Sequences (IsSequence)
import Data.String (String, IsString, fromString)
import Data.Word (Word8)
import GHC.Err (undefined)
import Language.Haskell.TH
import Test.Hspec hiding (shouldReturn)
import Test.Hspec qualified
import Text.Printf (IsChar, fromChar, toChar)
import Text.Show (Show, show)
import Data.Either
import System.IO (IO, print, putStr)
import System.Timeout (timeout)
import Data.Maybe (maybe)


instance IsString Name where fromString = mkName
instance IsString TypeQ where fromString = conT . mkName
instance IsString Lit where fromString = StringL
instance IsString ExpQ where fromString = litE . fromString

instance IsChar Word8 where
  fromChar = c2w
  toChar = w2c

monads :: [String]
monads =
  [ "IO"
  -- , "Either"
  ]

contexts :: [String]
contexts =
  [ "IndexContext"
  --, "UnixLC"
  --, "WindowsLC"
  --, "LinesOnly"
  --, "ColumnsOnly"
  --, "LineColumnUnknownBreak"
  --, "NoUpdate"
  ]

subStates :: [String]
subStates =
  [ "String"
  --, "StrictByteString"
  --, "LazyByteString"
  --, "StrictText"
  --, "LazyText"
  ]

instance Eq BB.Builder where x == y = BB.toLazyByteString x == BB.toLazyByteString y

limit :: IO a -> IO a
limit = (>>= maybe (fail "Timeout") return) . timeout 10000

shouldReturn :: (ShouldReturn m, HasCallStack, Show a, Eq a) => m a -> a -> Expectation
shouldReturn x y = limit $ shouldReturn' x y

class ShouldReturn m where shouldReturn' :: (HasCallStack, Show a, Eq a) => m a -> a -> Expectation
instance ShouldReturn IO where shouldReturn' x y = Test.Hspec.shouldReturn x y
instance Show a => ShouldReturn (Either a) where shouldReturn' x y = either (fail . ("Expected Right but received " <>) . show . Left @_ @()) (`shouldBe` y) x

