{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
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
  , module Biparse.Text.Context.LineColumn
  , module Biparse.Biparser
  , module Control.Monad.Writer
  , module Biparse.Biparser.StateWriter
  , module GHC.Float
  , module Data.Either
  , module Control.Monad.EitherString
  , module System.IO.Error
  , module Data.ByteString
  , module Data.Sequence
  , module Data.Vector
  , module Data.Kind
  , module Biparse.Context.IdentityState
  , module GHC.IO.Exception
  , module Data.Void
  , module Control.Monad.Except
  , module Control.Monad.StateError
  , module Data.MonoTraversable.Unprefixed
  , module Biparse.Text
  , module Control.Monad.ChangeMonad
  , module Biparse.Utils
  , module Data.Semigroup

  , fb
  , errorPosition
  , limit
  , FM
  ) where

import Biparse.Context.IdentityState (IdentityState)
import Data.Sequence (Seq)
import Data.Vector (Vector)
import Data.ByteString (ByteString)
import GHC.Float (Double)
import Biparse.Text.Context.LineColumn (LineColumn, LinesOnly, Position(Position,line,column), ErrorPosition(ErrorPosition))
import Biparse.Text (CharElement)
import Data.Sequences (drop, index, cons)
import Data.Functor.Identity (Identity(Identity, runIdentity))
import Data.Maybe (Maybe(Just,Nothing), maybe)
import Data.Int (Int)
import Data.Word (Word)
import Data.Semigroup ((<>))
import Data.Monoid (mempty)
import Data.Ord ((>))
import Biparse.General
import Data.Eq (Eq, (==), (/=))
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
import Data.Function ((.), ($), const, id)
import Control.Applicative (pure, (<|>), (<*), (*>), (<*>), empty, liftA2, Alternative)
import Data.Functor (Functor(fmap), (<$>), ($>), (<&>))
import Data.Bifunctor (first)
import Control.Monad.State (StateT(runStateT), get, put)
import Data.MonoTraversable (Element, headMay)
import Data.MonoTraversable.Unprefixed (toList)
import Data.Tuple (fst, snd)
import Biparse.Biparser hiding (Biparser, Iso, Unit, Const, ConstU)
import Control.Monad.Writer (WriterT(runWriterT))
import Biparse.Biparser.StateWriter (Biparser, Iso, Unit, Const, ConstU, runForward, runBackward, evalForward)
import Data.Either (Either(Left,Right), fromRight, isLeft)
import Control.Monad.EitherString (EitherString(EValue), isString)
import System.IO.Error (isUserError)
import Data.Kind (Type)
import GHC.IO.Exception (IOException)
import Data.Void (Void)
import Control.Monad.Except (MonadError(throwError,catchError))
import Control.Monad.StateError (ResultMonad(ResultingMonad), ErrorState)
import Biparse.Utils (headAlt)

import Control.Monad.ChangeMonad (ChangeMonad)
import System.Timeout (timeout)

fb :: forall is c s m m' n u v.
  ( m' ~ ResultingMonad m is
  , ChangeMonad is m m'
  , ResultMonad m is
  )
  => String
  -> Biparser c s m n u v
  -> ((s -> m' (v, s)) -> Spec)
  -> ((u -> n (v, SubState c s)) -> Spec)
  -> Spec
fb describeLabel bp fws bws = describe describeLabel do
  describe "forward" $ fws $ runForward @is bp
  describe "backward" $ bws $ runBackward bp

errorPosition :: Int -> Int -> Either ErrorPosition b -> Bool
errorPosition l c = \case
  Left (ErrorPosition l' c' _) -> l == l' && c == c'
  _ -> False

limit :: IO a -> IO a
limit = (>>= maybe (fail "Timeout") pure) . timeout 500

type FM text = Either (ErrorState String (Position text))

