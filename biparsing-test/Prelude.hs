{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC
  -Werror
  -Weverything

  -Wno-implicit-prelude
  -Wno-missing-deriving-strategies
  -Wno-missing-kind-signatures
  -Wno-missing-local-signatures
  -Wno-missing-safe-haskell-mode
  -Wno-safe
  -Wno-unsafe

  -Wno-orphans
  -Wno-missing-import-lists
#-}
module Prelude
  ( module Data.Bool
  , module Data.String
  , module System.IO
  , module System.IO.Error
  , module Data.Functor.Identity
  , module Data.Char
  , module Test.Hspec
  , module Test.Hspec.QuickCheck
  , module Test.QuickCheck
  , module Biparse.Biparser.StateReaderWriter
  , module Control.Applicative
  , module Biparse.Biparser
  , module Data.Function
  , module Control.Monad
  , module Data.Functor.Alt
  , module Data.Monoid
  , module Biparse.Text.Context.LineColumn
  , module Data.Either
  , module Control.Monad.EitherString
  , module Data.Int
  , module Data.Word
  , module Biparse.General
  , module GHC.Err
  , module Data.Sequences
  , module Data.ByteString.Internal
  , module Data.Functor
  , module Text.Printf
  , module Lens.Micro
  , module Control.Monad.Trans.RWS.CPS
  , module Control.Monad.StateError
  , module Numeric
  , module Numeric.Natural
  , module Data.Sequence
  , module Control.Monad.ChangeMonad
  , module Data.MonoTraversable
  , module Data.Eq
  , module GHC.Real
  , module Data.MonoTraversable.Unprefixed
  , module GHC.Enum
  , module Control.Monad.State.Class
  , module Control.Monad.Fail
  , module Data.Maybe
  , module Data.Tuple
  , module Safe
  , module GHC.Float
  , module Biparse.Utils
  , module Biparse.Context.Index
  , module Data.Vector
  , module Biparse.List
  , module GHC.Num
  , module Data.Ord
  , module Data.List.NonEmpty
  , module Text.Show
  , module Data.Default
  , module GHC.Generics
  , module Data.Coerce
  , module Data.List
  , module Data.Bifunctor
  , module Control.Monad.Error.Class
  , module Data.Semigroup
  , module GHC.Bits
  , module Control.Monad.Writer.Class

  , fb
  , EEP
  , EESP
  , errorPosition
  , errorPosition'
  , EIP
  , EISP
  , errorIndex
  , limit
  , FM
  , Data.ByteString.ByteString
  , Data.Text.Text
  , StrictByteString
  , StrictText
  , BuilderByteString
  , packStrictText
  , shouldReturn
  ) where

-- Exported
import Biparse.Biparser (UpdateStateWithElement(updateElementContext), UpdateStateWithSubState(updateSubStateContext), One, one, askBw, comap, upon, count, breakWhen', isNull, setBackward, try, peek, split, SubElement, IsoClass(iso))
import Biparse.Biparser.StateReaderWriter (Biparser, Iso, Unit, Const, ConstU, BackwardC(BackwardT,backwardT,runBackwardT), runForward, evalForward, runBackward)
import Biparse.Context.Index
import Biparse.General
import Biparse.List (all, takeElementsWhile)
import Biparse.Text.Context.LineColumn
import Biparse.Utils (headAlt, (<$$>))
import Control.Applicative (Applicative(pure,(<*>)), (*>), (<*), liftA2, empty)
import Control.Monad (Monad((>>=),return), when, sequence)
import Control.Monad.ChangeMonad (ChangeMonad(changeMonad'), Lift)
import Control.Monad.EitherString (EitherString, pattern EString, pattern EValue, isString, _EValue)
import Control.Monad.Error.Class (throwError, catchError)
import Control.Monad.Fail (MonadFail(fail))
import Control.Monad.State.Class (get, put)
import Control.Monad.StateError (StateErrorT, ErrorState(ErrorState), ErrorInstance(ErrorStateInstance))
import Control.Monad.Trans.RWS.CPS (RWST, mapRWST, rwsT, runRWST)
import Control.Monad.Writer.Class (MonadWriter)
import Data.Bifunctor (first, second)
import Data.Bool (Bool(True,False), (&&), otherwise, bool)
import Data.ByteString.Internal (c2w, w2c)
import Data.Char (Char, isDigit)
import Data.Coerce (coerce)
import Data.Default (Default(def))
import Data.Either (Either(Right), isRight, either)
import Data.Eq (Eq((==)), (/=))
import Data.Function
import Data.Functor (Functor, (<$>), (<&>), ($>))
import Data.Functor.Alt (Alt((<!>)))
import Data.Functor.Identity (Identity(Identity,runIdentity))
import Data.Int (Int)
import Data.List (zip)
import Data.List.NonEmpty (NonEmpty)
import Data.Maybe (Maybe(Just,Nothing), maybe)
import Data.MonoTraversable (Element)
import Data.MonoTraversable.Unprefixed (length)
import Data.Monoid (Monoid(mempty))
import Data.Ord
import Data.Semigroup (Semigroup((<>)))
import Data.Sequence (Seq)
import Data.Sequences (IsSequence, Index, cons, snoc, singleton)
import Data.String (String, IsString(fromString))
import Data.Tuple (fst, snd)
import Data.Vector (Vector)
import Data.Word (Word)
import GHC.Bits (Bits)
import GHC.Enum (Enum(succ))
import GHC.Err (undefined)
import GHC.Float (Double)
import GHC.Generics (Generic)
import GHC.Num (Num, (+), (-))
import GHC.Real (Fractional, Integral, fromIntegral, Real)
import Lens.Micro ((^.), (%~), _1, _2, _3)
import Numeric (showHex)
import Numeric.Natural (Natural)
import Safe (headMay)
import System.IO (IO, FilePath)
import System.IO.Error (isUserError, ioeGetErrorString, userError)
import Test.Hspec hiding (shouldReturn)
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Test.QuickCheck.Instances.Text ()
import Text.Printf (IsChar(fromChar,toChar))
import Text.Show (Show(show))

-- Internal
import Control.Monad.State (StateT)
import Control.Monad.Writer.CPS (WriterT)
import Control.Monad.Trans.State.Selectable (StateTransformer)
import Control.Monad.Trans.Writer.Selectable (WriterTransformer)
import Data.ByteString qualified
import Data.ByteString.Builder qualified
import Data.Convert (ConvertSequence(convertSequence))
import Data.Either (Either(Left))
import Data.Text qualified
import Data.Word (Word8)
import GHC.Exts (IsList, fromList, Item)
import GHC.Exts qualified
import System.Timeout (timeout)
import Test.Hspec qualified

fb :: forall c s m n r w ws u v.
  ( Functor n
  , BackwardC c n w
  )
  => String
  -> Biparser c s m n r w ws u v
  -> r
  -> ws
  -> ((s -> m (v, s)) -> Spec)
  -> ((u -> n (v, w)) -> Spec)
  -> Spec
fb describeLabel bp r ws fws bws = describe describeLabel do
  describe "forward" $ fws $ runForward bp
  describe "backward" $ bws \u -> runBackward bp r ws u

type EEP dataId e text = Either (ErrorState e (Position dataId text))
type EESP dataId text = EEP dataId String text

errorPosition :: Eq e => Int -> Int -> EEP () e text b -> Bool
errorPosition = errorPosition' Nothing

errorPosition' :: Eq e => Maybe e -> Int -> Int -> EEP () e text b -> Bool
errorPosition' me l c = \case
  Left (ErrorState e (Position _ l' c' _)) -> l == l' && c == c' && maybe True (== e) me
  _ -> False

type EIP e ss = Either (ErrorState e (IndexPosition ss))
type EISP ss = EIP String ss

errorIndex :: (Eq i, i ~ Index ss) => i -> Either (ErrorState e (IndexPosition ss)) b -> Bool
errorIndex i = \case
  Left (ErrorState _ (IndexPosition i' _)) -> i == i'
  _ -> False

limit :: IO a -> IO a
limit = (>>= maybe (fail "Timeout") pure) . timeout 100000

type FM text = Either (ErrorState String (Position () text))

instance IsChar Word8 where
  fromChar = c2w
  toChar = w2c

instance IsList a => IsList (Identity a) where
  type Item (Identity a) = Item a
  fromList = Identity . fromList
  toList = GHC.Exts.toList . runIdentity

type StrictText = Data.Text.Text
type StrictByteString = Data.ByteString.ByteString
type BuilderByteString = Data.ByteString.Builder.Builder

packStrictText :: String -> StrictText
packStrictText = Data.Text.pack

instance Eq BuilderByteString where x == y = Data.ByteString.Builder.toLazyByteString x == Data.ByteString.Builder.toLazyByteString y

instance Applicative m => ConvertSequence c String StrictText m where convertSequence = pure . fromString
instance Applicative m => ConvertSequence c String StrictByteString m where convertSequence = pure . fromString

instance (Functor n, Monoid w) => BackwardC c n w where
  type BackwardT c = RWST
  backwardT = rwsT
  runBackwardT = runRWST

instance UpdateStateWithElement () (Identity ss) where
  updateElementContext _ _ = Identity
instance UpdateStateWithSubState () (Identity ss) where
  updateSubStateContext _ _ = Identity

type instance StateTransformer _ = StateT
type instance WriterTransformer _ = WriterT

shouldReturn :: (ShouldReturn m, HasCallStack, Show a, Eq a) => m a -> a -> Expectation
shouldReturn x y = limit $ shouldReturn' x y
class ShouldReturn m where shouldReturn' :: (HasCallStack, Show a, Eq a) => m a -> a -> Expectation
instance ShouldReturn IO where shouldReturn' x y = Test.Hspec.shouldReturn x y
instance Show a => ShouldReturn (Either a) where shouldReturn' x y = either (fail . ("Expected Right but received " <>) . show . Left @_ @()) (`shouldBe` y) x

