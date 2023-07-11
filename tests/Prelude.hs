{-# LANGUAGE NoImplicitPrelude #-}
module Prelude
  ( module GHC.Err
  , module System.IO
  , module Test.Hspec
  , module Test.Hspec.QuickCheck
  , module Test.QuickCheck
  , module Text.Show
  , module Data.String
  , module Data.Char
  , module Data.Bool
  , module Control.Monad
  , module Data.List
  , module Data.Eq
  , module Data.Text
  , module Data.Ord
  , module Biparse.General
  , module Data.Monoid
  , module Data.Function
  , module Data.Maybe
  , module Data.Int
  , module Data.Word
  , module Data.Functor
  , module Control.Applicative
  , module Data.Bifunctor
  , module Data.Functor.Identity
  , module Data.Sequences
  , module Control.Monad.State
  , module Data.MonoTraversable
  , module Data.Tuple
  , module Biparse.Text.PositionContext
  , module Biparse.Biparser
  , module Control.Monad.Writer
  , module Biparse.Biparser.StateWriter
  , module GHC.Float
  , module Data.Either
  , module System.IO.Error
  , module Data.ByteString
  , module Data.Sequence
  , module Data.Vector
  , module Data.Kind

  , fb
  , errorPosition
  , limit
  , FM
  ) where

import Data.Sequence (Seq)
import Data.Vector (Vector)
import Data.ByteString (ByteString)
import GHC.Float (Double)
import Biparse.Text.PositionContext (LineColumn, LinesOnly, Position(Position,line,column))
import Data.Sequences (drop, index)
import Data.Functor.Identity (Identity(Identity, runIdentity))
import Data.Maybe (Maybe(Just,Nothing), maybe)
import Data.Int (Int)
import Data.Word (Word)
import Data.Monoid (mempty)
import Data.Ord ((>))
import Biparse.General
import Data.Eq ((==), (/=))
import Data.Text (Text)
import Data.Char (Char, isDigit)
import Data.Bool (Bool(True,False), otherwise, (&&))
import Data.List ((++))
import GHC.Err (undefined)
import Data.String (String, IsString(fromString))
import System.IO (IO, FilePath)
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Test.QuickCheck.Instances.Text ()
import Text.Show (Show(show))
import Control.Monad ((>>=), return, (>>), fail, MonadPlus)
import Data.Function ((.), ($), const)
import Control.Applicative (pure, (<|>), (<*), (*>), (<*>), empty, liftA2)
import Data.Functor (Functor(fmap), (<$>), ($>))
import Data.Bifunctor (first)
import Control.Monad.State (StateT(runStateT), get, put)
import Data.MonoTraversable (headMay)
import Data.Tuple (fst, snd)
import Biparse.Biparser hiding (Biparser, Iso, Unit, Const, ConstU)
import Control.Monad.Writer (WriterT(runWriterT))
import Biparse.Biparser.StateWriter (Biparser, Iso, Unit, Const, ConstU, runForward, runBackward, evalForward)
import Data.Either (Either(Left,Right), fromRight)
import System.IO.Error (isUserError)
import Data.Kind (Type)

import System.Timeout (timeout)

fb :: forall c s m n u v.
  (
  )
  => String
  -> Biparser c s m n u v
  -> ((s -> m (v, s)) -> Spec)
  -> ((u -> n (v, SubState c s)) -> Spec)
  -> Spec
fb describeLabel bp fws bws = describe describeLabel do
  describe "forward" $ fws $ runForward bp
  describe "backward" $ bws $ runBackward bp

errorPosition :: Int -> Int -> Either (Position a) b -> Bool
errorPosition l c = \case
  Left (Position {line,column}) -> l == line && c == column
  _ -> False


limit :: IO a -> IO a
limit = (>>= maybe (fail "Timeout") pure) . timeout 500

type FM = Either (Position String)

