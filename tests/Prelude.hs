{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module Prelude
  ( module Biparse.Biparser
  , module Biparse.Biparser.StateReaderWriter
  , module Biparse.General
  , module Biparse.Text
  , module Biparse.Text.Context.LineColumn
  , module Biparse.Utils
  , module Control.Applicative
  , module Control.Monad
  , module Control.Monad.ChangeMonad
  , module Control.Monad.EitherString
  , module Control.Monad.Except
  , module Control.Monad.State
  , module Control.Monad.StateError
  , module Control.Monad.Writer
  , module Data.Bifunctor
  , module Data.Bool
  , module Data.ByteString
  , module Data.Char
  , module Data.Either
  , module Data.Eq
  , module Data.Function
  , module Data.Functor
  , module Data.Functor.Identity
  , module Data.Int
  , module Data.Kind
  , module Data.List
  , module Data.Maybe
  , module Data.MonoTraversable
  , module Data.MonoTraversable.Unprefixed
  , module Data.Monoid
  , module Data.Ord
  , module Data.Semigroup
  , module Data.Sequence
  , module Data.Sequences
  , module Data.String
  , module Data.Text
  , module Data.Tuple
  , module Data.Vector
  , module Data.Void
  , module Data.Word
  , module GHC.Err
  , module GHC.Float
  , module GHC.IO.Exception
  , module System.IO
  , module System.IO.Error
  , module Test.Hspec
  , module Test.Hspec.QuickCheck
  , module Test.QuickCheck
  , module Text.Show
  , module Numeric.Natural
  , module GHC.Num
  , module GHC.Generics
  , module Data.Coerce
  , module Data.ByteString.Internal
  , module Biparse.Context.Index
  , module Control.Monad.RWS
  , module Control.Monad.Trans.Class
  , module GHC.Real

  , fb
  , errorPosition
  , errorIndex
  , limit
  , FM
  , TriSum(..)
  , (>>>)
  ) where

import Data.ByteString.Internal (w2c, c2w)
import Biparse.Biparser hiding (Biparser, Iso, Unit, Const, ConstU)
import Biparse.Biparser.StateReaderWriter (Biparser, Iso, Unit, Const, ConstU, runForward, runBackward, evalForward)
import Biparse.General
import Biparse.Text (CharElement)
import Biparse.Text.Context.LineColumn (LineColumn, UnixLC, LinesOnly, ColumnsOnly, Position(Position), subState, ErrorPosition(ErrorPosition), startLineColumn, NoUpdate)
import Biparse.Context.Index (IndexContext, IndexPosition(IndexPosition), ErrorIndex(ErrorIndex), EISP)
import Biparse.Utils (headAlt)
import Control.Applicative (pure, (<|>), (<*), (*>), (<*>), empty, liftA2, Alternative)
import Control.Monad ((>>=), return, (>>), fail, MonadPlus, MonadFail, Monad)
import Control.Monad.EitherString (EitherString(EValue), isString)
import Control.Monad.Except (MonadError(throwError,catchError))
import Control.Monad.State (MonadState, get, put)
import Control.Monad.StateError (StateErrorT, ErrorInstance(NewtypeInstance,ErrorStateInstance), ErrorState(ErrorState))
import Control.Monad.Writer (WriterT(runWriterT), MonadWriter)
import Data.Bifunctor (first)
import Data.Bool (Bool(True,False), otherwise, (&&), bool)
import Data.ByteString (ByteString)
import Data.Char (Char, isDigit)
import Data.Either (Either(Left,Right), fromRight, isLeft, isRight, either)
import Data.Eq (Eq, (==), (/=))
import Data.Function ((.), ($), const, id, flip, (&))
import Data.Functor (Functor(fmap), (<$>), ($>), (<&>))
import Data.Functor.Identity (Identity(Identity, runIdentity))
import Data.Int (Int)
import Data.Kind (Type)
import Data.List ((++))
import Data.Maybe (Maybe(Just,Nothing), maybe)
import Data.MonoTraversable (Element, headMay)
import Data.MonoTraversable.Unprefixed (toList, length)
import Data.Monoid (Monoid, mempty)
import Data.Ord (Ord, (>), (>=))
import Data.Semigroup (Semigroup, (<>))
import Data.Sequence (Seq)
import Data.Sequences (drop, index, cons, snoc, replicate, IsSequence, Index)
import Data.String (String, IsString(fromString))
import Data.Text (Text)
import Data.Tuple (fst, snd)
import Data.Vector (Vector)
import Data.Void (Void)
import Data.Word (Word, Word8)
import GHC.Err (undefined)
import GHC.Float (Double)
import GHC.IO.Exception (IOException)
import System.IO (IO, FilePath)
import System.IO.Error (isUserError)
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Test.QuickCheck.Instances.Text ()
import Text.Show (Show(show))
import Numeric.Natural (Natural)
import GHC.Num ((+), (-))
import GHC.Generics (Generic)
import Data.Coerce (coerce)
import Control.Monad.RWS (RWST)
import Control.Monad.ChangeMonad (ChangeMonad, ChangeFunction, changeMonad', ResultMonad(ResultingMonad))
import Control.Monad.Trans.Class (MonadTrans, lift)
import GHC.Real (fromIntegral)

import GHC.Exts (IsList, fromList, Item)
import GHC.Exts qualified
import Text.Printf (IsChar, fromChar, toChar)
import System.Timeout (timeout)

fb :: forall is c s m m' n r ws u v.
  ( m' ~ ResultingMonad m is
  , ChangeMonad is m m'
  , ResultMonad m is
  , Functor n
  )
  => String
  -> Biparser c s m n r ws u v
  -> r
  -> ws
  -> ((s -> m' (v, s)) -> Spec)
  -> ((u -> n (v, SubState s)) -> Spec)
  -> Spec
fb describeLabel bp r ws fws bws = describe describeLabel do
  describe "forward" $ fws $ runForward @is bp
  describe "backward" $ bws \u -> runBackward bp r ws u

errorPosition :: Int -> Int -> Either (ErrorPosition ()) b -> Bool
errorPosition l c = \case
  Left (ErrorPosition () l' c' _) -> l == l' && c == c'
  _ -> False

errorIndex :: (Eq i, i ~ Index ss) => i -> Either (ErrorIndex ss) b -> Bool
errorIndex i = \case
  Left (ErrorIndex i' _) -> i == i'
  _ -> False

limit :: IO a -> IO a
limit = (>>= maybe (fail "Timeout") pure) . timeout 100000

type FM text = Either (ErrorState String (Position () text))

data TriSum a b c = One a | Two b | Three c deriving (Show, Eq)
instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (TriSum a b c) where
  arbitrary = oneof [One <$> arbitrary, Two <$> arbitrary, Three <$> arbitrary]
  shrink = \case
    One x -> One <$> shrink x
    Two x -> Two <$> shrink x
    Three x -> Three <$> shrink x

infixr 9 >>>
(>>>) :: (a -> b) -> (b -> c) -> a -> c
(>>>) = flip (.)

instance IsChar Word8 where
  fromChar = c2w
  toChar = w2c

instance IsList a => IsList (Identity a) where
  type Item (Identity a) = Item a
  fromList = Identity . fromList
  toList = GHC.Exts.toList . runIdentity

