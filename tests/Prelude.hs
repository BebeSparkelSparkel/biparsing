{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module Prelude
  ( module Biparse.Biparser
  , module Biparse.Biparser.StateReaderWriter
  , module Biparse.Context.Index
  , module Biparse.General
  , module Biparse.Text
  , module Biparse.Text.Context.LineColumn
  , module Biparse.Utils
  , module Control.Applicative
  , module Control.Monad
  , module Control.Monad.ChangeMonad
  , module Control.Monad.EitherString
  , module Control.Monad.Except
  , module Control.Monad.RWS
  , module Control.Monad.State
  , module Control.Monad.StateError
  , module Control.Monad.Trans.Class
  , module Control.Monad.Writer
  , module Data.Bifunctor
  , module Data.Bool
  , module Data.ByteString
  , module Data.ByteString.Builder
  , module Data.ByteString.Internal
  , module Data.Char
  , module Data.Coerce
  , module Data.Either
  , module Data.Eq
  , module Data.Function
  , module Data.Functor
  , module Data.Functor.Alt
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
  , module GHC.Generics
  , module GHC.IO.Exception
  , module GHC.Num
  , module GHC.Real
  , module Numeric.Natural
  , module System.IO
  , module System.IO.Error
  , module Test.Hspec
  , module Test.Hspec.QuickCheck
  , module Test.QuickCheck
  , module Text.Show

  , fb
  , errorPosition
  , errorIndex
  , limit
  , FM
  , TriSum(..)
  , (>>>)
  ) where

import Biparse.Biparser hiding (Biparser, Iso, Unit, Const, ConstU)
import Biparse.Biparser.StateReaderWriter (Biparser, Iso, Unit, Const, ConstU, runForward, runBackward, evalForward)
import Biparse.Context.Index (IndexContext, IndexPosition(IndexPosition), ErrorIndex(ErrorIndex), EISP)
import Biparse.General
import Biparse.Text (CharElement)
import Biparse.Text.Context.LineColumn (LineColumn, UnixLC, LinesOnly, ColumnsOnly, Position(Position), subState, ErrorPosition(ErrorPosition), startLineColumn, NoUpdate)
import Biparse.Utils (headAlt, ConvertSequence, convertSequence)
import Control.Applicative (Applicative, pure, (<*), (*>), (<*>), empty, liftA2)
import Control.Monad ((>>=), return, (>>), fail, MonadPlus, MonadFail, Monad, void)
import Control.Monad.ChangeMonad (ChangeMonad, ChangeFunction, changeMonad', ResultMonad(ResultingMonad))
import Control.Monad.EitherString (EitherString(EValue), isString)
import Control.Monad.Except (MonadError(throwError,catchError))
import Control.Monad.RWS (RWST)
import Control.Monad.State (MonadState, get, put)
import Control.Monad.StateError (StateErrorT, ErrorInstance(NewtypeInstance,ErrorStateInstance), ErrorState(ErrorState))
import Control.Monad.Trans.Class (MonadTrans, lift)
import Control.Monad.Writer (WriterT(runWriterT), MonadWriter)
import Data.Bifunctor (first)
import Data.Bool (Bool(True,False), otherwise, (&&), bool)
import Data.ByteString (ByteString)
import Data.ByteString.Builder (Builder)
import Data.ByteString.Internal (w2c, c2w)
import Data.Char (Char, isDigit)
import Data.Coerce (coerce)
import Data.Either (Either(Left,Right), fromRight, isLeft, isRight, either)
import Data.Eq (Eq, (==), (/=))
import Data.Function ((.), ($), const, id, flip, (&))
import Data.Functor (Functor(fmap), (<$>), ($>), (<&>))
import Data.Functor.Alt (Alt, (<!>))
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
import GHC.Generics (Generic)
import GHC.IO.Exception (IOException)
import GHC.Num ((+), (-))
import GHC.Real (fromIntegral)
import Numeric.Natural (Natural)
import System.IO (IO, FilePath)
import System.IO.Error (isUserError)
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Test.QuickCheck.Instances.Text ()
import Text.Show (Show(show))

import GHC.Exts (IsList, fromList, Item)
import GHC.Exts qualified
import Text.Printf (IsChar, fromChar, toChar)
import System.Timeout (timeout)
import Data.ByteString.Builder qualified

fb :: forall is c s m m' n r w ws u v.
  ( m' ~ ResultingMonad m is
  , ChangeMonad is m m'
  , ResultMonad m is
  , Functor n
  )
  => String
  -> Biparser c s m n r w ws u v
  -> r
  -> ws
  -> ((s -> m' (v, s)) -> Spec)
  -> ((u -> n (v, w)) -> Spec)
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

instance Eq Builder where x == y = Data.ByteString.Builder.toLazyByteString x == Data.ByteString.Builder.toLazyByteString y

instance Applicative m => ConvertSequence c String Text       m where convertSequence = pure . fromString
instance Applicative m => ConvertSequence c String ByteString m where convertSequence = pure . fromString

