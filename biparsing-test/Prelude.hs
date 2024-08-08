{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC
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
module Prelude (
module Export,

run,
ConstructParameter,
TestParameters(..),
Direction(..),
Forward,
Backward,
Read,
State,
ShowStM',
EqStM',
ShouldFailQ,
RunBase(BaseMonad, StM'),
MakeForwardResult,
MakeBackwardResult,
MakeResult(..),
--RunBackward(..),
--MakeBackwardResult(..),
ByteStringBuilder,
TextBuilder,
ShouldReturnQ,
ShouldReturn,
shouldReturn,
ShouldFailQ,
ShouldFail,
shouldFail,
) where

import Biparse.Comap as Export
import Biparse.Control.Bwd as Export
import Biparse.Control.File as Export (FileT, OpenFile(openFile), OpenFrom, MonadFileGetChar)
import Biparse.Control.Fwd as Export
import Biparse.Core.Aliases as Export
import Biparse.Core.Classes as Export
import Biparse.Core.Update as Export (UpdateStateWithElement)
import Biparse.General as Export
import Biparse.State.Index as Export (IndexPosition(IndexPosition))
import Biparse.Text.State.LineColumn as Export
import Control.Applicative as Export (Applicative(pure,(<*>)), (*>), (<*), liftA2, empty)
import Control.Monad as Export (Monad((>>=),return), when, sequence)
import Control.Monad.Catch as Export (MonadMask)
import Control.Monad.Error.Class as Export (throwError, catchError)
import Control.Monad.Fail as Export (MonadFail(fail))
import Control.Monad.IO.Class as Export (liftIO)
import Control.Monad.Identity as Export (IdentityT(runIdentityT))
import Control.Monad.Reader as Export (ReaderT)
import Control.Monad.State.Class as Export (MonadState(get,put))
import Control.Monad.Writer.Class as Export (MonadWriter(tell))
import Data.Bifunctor as Export (first, second)
import Data.Bool as Export (Bool(True,False), (&&), otherwise, bool)
import Data.ByteString as Export (StrictByteString)
import Data.ByteString.Internal as Export (c2w, w2c)
import Data.ByteString.Lazy as Export (ByteString, LazyByteString)
import Data.Char as Export (Char, isDigit)
import Data.Coerce as Export (coerce)
import Data.Default as Export (Default(def))
import Data.Either as Export (Either(Right), isLeft, isRight, either)
import Data.Eq as Export (Eq((==)), (/=))
import Data.Function as Export
import Data.Functor as Export (Functor, (<$>), (<&>), ($>), fmap)
import Data.Functor.Alt as Export (Alt((<!>)))
import Data.Functor.Identity as Export (Identity(Identity,runIdentity))
import Data.Int as Export
import Data.Kind as Export (Type, Constraint)
import Data.List as Export (zip)
import Data.List.NonEmpty as Export (NonEmpty)
import Data.Maybe as Export (Maybe(Just,Nothing), maybe)
import Data.MonoTraversable as Export (Element, olength)
import Data.Monoid as Export (Monoid(mempty))
import Data.Ord as Export
import Data.Profunctor as Export (Profunctor)
import Data.Proxy as Export
import Data.Semigroup as Export (Semigroup((<>)))
import Data.Sequence as Export (Seq)
import Data.Sequences as Export (Index, IsSequence)
import Data.String as Export (String, IsString(fromString))
import Data.Text as Export (Text, StrictText)
import Data.Text.Lazy as Export (LazyText)
import Data.Tuple as Export (fst, snd, uncurry)
import Data.Vector as Export (Vector)
import Data.Word as Export (Word8)
import GHC.Bits as Export (Bits)
import GHC.Enum as Export (Enum(succ), maxBound)
import GHC.Err as Export (undefined)
import GHC.Float as Export (Double)
import GHC.Generics as Export (Generic(from,to))
import GHC.Num as Export (Num, (+), (-))
import GHC.Real as Export (Fractional, Integral, fromIntegral, Real, div)
import Lens.Micro as Export ((^.), (.~), (%~), _1, _2, _3)
import Numeric as Export (showHex)
import Numeric.Natural as Export (Natural)
import System.IO as Export (IO, FilePath)
import System.IO.Error as Export (isUserError, ioeGetErrorString, userError)
import Test.Hspec as Export hiding (shouldReturn)
import Test.Hspec.QuickCheck as Export
import Test.QuickCheck as Export
import Test.QuickCheck.Instances.Text as Export ()
import Text.Printf as Export (IsChar(fromChar,toChar))
import Text.Show as Export (Show(show))
import Type.Reflection as Export (Typeable, typeRep)

-- Internal Imports
import Data.Maybe as Export (isNothing)
import Control.Monad.Reader (runReaderT)
import Control.Monad.IO.Class as Export (MonadIO)
import Control.Monad.Identity (runIdentityT)
import Control.Monad.State.Class (MonadState)
import Biparse.Control.File (runFileT, OpenWith)
import Biparse.State.Lenses (HasDataId)
--import Biparse.Control.StateError (runStateErrorT)
import Control.Monad.Writer (runWriterT)
import Data.ByteString.Builder qualified
import Data.ByteString.Builder.Internal (byteStringInsert, byteStringThreshold, toLazyByteStringWith, safeStrategy, smallChunkSize)
import Data.Either (Either(Left))
import Data.List (elem)
--import Data.List (foldr)
import Data.MonoTraversable (MonoPointed(opoint))
import Data.Sequences (cons, snoc)
import Data.Text.Lazy.Builder qualified
import GHC.Exts (IsList(..))
import System.IO.Unsafe (unsafePerformIO)
import Test.Hspec qualified
--import Test.Hspec.Core.Spec (SpecM)
import System.IO.Temp (withSystemTempFile)
import System.IO (openFile, openTempFile, hSeek, hGetContents, SeekMode(AbsoluteSeek), IOMode(ReadMode, WriteMode, ReadWriteMode), withFile, hClose)
import System.IO qualified
import Biparse.Core.Update (updateStateWithElement)
import Control.Monad.Trans.State.Lazy qualified
import Control.Monad.Trans.State.Strict qualified
import Control.Monad.Trans.Writer.CPS qualified
import Control.Monad.Trans.Writer.Lazy qualified
import Control.Monad.Trans.Writer.Strict qualified
import Control.Monad.Trans.RWS.CPS qualified
import Control.Monad.Trans.RWS.Lazy qualified
import Control.Monad.Trans.RWS.Strict qualified
import Control.Monad.Trans.Control

run :: forall d p r s u v a.
  ( RunBase (TestParameters d r s a) (p u)
  , ConstructParameter a (TestParameters d r s a)
  ) => Biparser p u v -> FilePath -> a -> BaseResult (p u) v
run bp fp x = runBase @(TestParameters d r s a) bp $ constructParameter fp x

class ConstructParameter a b where constructParameter :: FilePath -> a -> b
instance (ConstructParameter a r, ConstructParameter a s) => ConstructParameter a (TestParameters d r s a) where
  constructParameter filePath parameter = TestParameters {read = constructParameter filePath parameter, state = constructParameter filePath parameter, filePath, parameter}
instance (ConstructParameter String s, IsString text) => ConstructParameter String (StateSeq s text) where
  constructParameter filePath string = StateSeq (constructParameter filePath string) (fromString string)
instance IsString dataId => ConstructParameter a (Position context dataId) where
  constructParameter filePath _ = (def :: Position context ()) & dataId .~ fromString filePath
instance IsString dataId => ConstructParameter a (IndexPosition dataId) where
  constructParameter filePath _ = (def :: IndexPosition ()) & dataId .~ fromString filePath
instance ConstructParameter a () where
  constructParameter _ _ = ()

type BaseResult m a = BaseMonad m (StM' m a)
--newtype BaseResult m a = BaseResult {unBaseResult :: BaseMonad m (StM' m a)}

data TestParameters (direction :: Direction) read state a = TestParameters
  { filePath :: FilePath
  , read :: read
  , state :: state
  , parameter :: a
  }

data Direction = Forward | Backward
data Forward a
type instance Element (Forward text) = Element text
type instance OpenWith (Forward text) = OpenWith text
data Backward a
type instance Element (Backward text) = Element text
type instance OpenWith (Backward text) = OpenWith text

type Read :: (Type -> Type) -> Type
type family Read m
type instance Read (Fwd m u) = Read m
type instance Read (FileT text m) = Read m
type instance Read (IdentityT m) = Read m
type instance Read (ReaderT r m) = r
type instance Read (LazyWriterT _ m) = Read m
type instance Read (LazyStateT _ m) = Read m
type instance Read (LazyRWST r _ _ _) = r
type instance Read IO = ()
type instance Read Maybe = ()
type instance Read Identity = ()

type State :: (Type -> Type) -> Type
type family State m
type instance State (Fwd m u) = State m
type instance State (FileT text m) = State m
type instance State (IdentityT m) = State m
type instance State (ReaderT _ m) = State m
type instance State (LazyWriterT _ m) = State m
type instance State (LazyStateT s _) = s
type instance State (LazyRWST _ _ s _) = s
type instance State IO = ()
type instance State Maybe = ()
type instance State Identity = ()

-- | Quantified Constraint Trick: https://blog.poisson.chat/posts/2022-09-21-quantified-constraint-trick.html
class Show (StM' m a) => ShowStM' m a
instance Show (StM' m a) => ShowStM' m a
class Eq (StM' m a) => EqStM' m a
instance Eq (StM' m a) => EqStM' m a
class ShouldReturn (BaseMonad (p u)) => ShouldReturnQ p u
instance ShouldReturn (BaseMonad (p u)) => ShouldReturnQ p u
class ShouldFail (BaseMonad (p u)) => ShouldFailQ p u
instance ShouldFail (BaseMonad (p u)) => ShouldFailQ p u
class MakeResult (Position () FilePath -> IndexPosition FilePath -> String -> a -> StM' m v) => MakeForwardResult m a v
instance MakeResult (Position () FilePath -> IndexPosition FilePath -> String -> a -> StM' m v) => MakeForwardResult m a v
class MakeResult (String -> u -> StM' m v) => MakeBackwardResult u m v
instance MakeResult (String -> u -> StM' m v) => MakeBackwardResult u m v

class RunBase b m where
  type BaseMonad m :: Type -> Type
  type StM' m a :: Type
  runBase :: m a -> b -> BaseResult m a
instance RunBase b m => RunBase b (Fwd m u) where
  type BaseMonad (Fwd m _) = BaseMonad m
  type StM' (Fwd m _) a = StM' m a
  runBase x b = runBase (runFwd x) b
instance RunBase (TestParameters d r s u) m => RunBase (TestParameters d r s u) (Bwd m u) where
  type BaseMonad (Bwd m _) = BaseMonad m
  type StM' (Bwd m _) a = StM' m a
  runBase x b@(TestParameters {parameter = u}) = runBase (runBwd x u) b
instance
  ( OpenFrom text
  , MonadIO m
  , MonadMask m
  , RunBase (TestParameters 'Forward r s String) m
  , BaseMonad m ~ IO
  ) => RunBase (TestParameters 'Forward r s String) (FileT (Forward text) m) where
  type BaseMonad (FileT _ _) = IO
  type StM' (FileT (Forward _) m) a = StM' m a
  runBase x b@(TestParameters {filePath, parameter = string}) = withSystemTempFile filePath \fp h -> do
    liftIO $ System.IO.hPutStr h string *> hClose h
    runBase (runFileT x fp ReadWriteMode) b
instance
  ( OpenFrom text
  , MonadIO m
  , MonadMask m
  , RunBase (TestParameters 'Backward r s u) m
  , BaseMonad m ~ IO
  , IsString text
  ) => RunBase (TestParameters 'Backward r s u) (FileT (Backward text) m) where
  type BaseMonad (FileT _ _) = IO
  type StM' (FileT (Backward text) m) a = (StM' m a, text)
  runBase x b@(TestParameters {filePath, parameter = string}) = withSystemTempFile filePath \fp h -> do
    hClose h
    y <- runBase (runFileT x fp ReadWriteMode) b
    str <- System.IO.readFile fp
    pure (y, fromString str)
instance RunBase b m => RunBase b (IdentityT m) where
  type BaseMonad (IdentityT m) = BaseMonad m
  type StM' (IdentityT m) a = StM' m a
  runBase x b = runBase (runIdentityT x) b
instance RunBase (TestParameters d r s a) m => RunBase (TestParameters d r s a) (ReaderT r m) where
  type BaseMonad (ReaderT _ m) = BaseMonad m
  type StM' (ReaderT _ m) a = StM' m a
  runBase x b@(TestParameters {read}) = runBase (runReaderT x read) b
instance RunBase (TestParameters d r s a) m => RunBase (TestParameters d r s a) (LazyWriterT w m) where
  type BaseMonad (LazyWriterT _ m) = BaseMonad m
  type StM' (LazyWriterT w m) a = StM' m (a, w)
  runBase x b = runBase (Control.Monad.Trans.Writer.Lazy.runWriterT x) b
instance RunBase (TestParameters d r s a) m => RunBase (TestParameters d r s a) (LazyStateT s m) where
  type BaseMonad (LazyStateT s m) = BaseMonad m
  type StM' (LazyStateT s m) a = StM' m (a, s)
  runBase x b@(TestParameters {state}) = runBase (Control.Monad.Trans.State.Lazy.runStateT x state) b
instance RunBase (TestParameters d r s a) m => RunBase (TestParameters d r s a) (LazyRWST r w s m) where
  type BaseMonad (LazyRWST _ _ _ m) = BaseMonad m
  type StM' (LazyRWST _ w s m) a = StM' m (a, s, w)
  runBase x b@(TestParameters {read, state}) = runBase (Control.Monad.Trans.RWS.Lazy.runRWST x read state) b
instance RunBase b IO where
  type BaseMonad IO = IO
  type StM' IO a = a
  runBase = const
instance RunBase b Maybe where
  type BaseMonad Maybe = Maybe
  type StM' Maybe a = a
  runBase = const

class MakeResult a where makeResult :: a
instance MakeResult (p -> i -> str -> v -> v) where
  makeResult _ _ _ x = x
instance MakeResult (p -> i -> str -> v -> (v, p)) where
  makeResult p _ _ x = (x, p)
instance MakeResult (p -> i -> str -> v -> (v, i)) where
  makeResult _ i _ x = (x, i)
instance (IsString text, IsString dataId) => MakeResult (Position () FilePath -> i -> String -> v -> (v, StateSeq (Position context dataId) text)) where
  makeResult p _ s x = (x, StateSeq (coerce $ p & dataId %~ fromString @dataId) (fromString s))
instance (IsString text, IsString dataId) => MakeResult (p -> IndexPosition FilePath -> String -> v -> (v, StateSeq (IndexPosition dataId) text)) where
  makeResult _ i s x = (x, (StateSeq (coerce $ i & dataId %~ fromString @dataId) (fromString s)))
instance IsString text => MakeResult (p -> i -> String -> v -> (v, StateSeq () text)) where
  makeResult _ i s x = (x, (StateSeq () (fromString s)))
instance MakeResult (p -> i -> str -> v -> v) => MakeResult (p -> i -> str -> v -> (v, ())) where
  makeResult p i s v = (makeResult p i s v, ())
instance MakeResult (p -> i -> str -> v -> (v, s)) => MakeResult (p -> i -> str -> v -> ((v, ()), s)) where
  makeResult p i s v = first (, ()) $ makeResult p i s v
instance MakeResult (p -> i -> str -> v -> (v, s)) => MakeResult (p -> i -> str -> v -> (v, s, ())) where
  makeResult p i s v = makeResult p i s v & \(v, s) -> (v, s, ())

instance IsString text => MakeResult (String -> v -> (v, text)) where
  makeResult s v = (v, fromString s)
instance MakeResult (s -> v -> (v, text)) => MakeResult (s -> v -> (v, (), text)) where
  makeResult s v = makeResult s v & \(x, y) -> (x, (), y)
instance MakeResult (s -> v -> (v, text)) => MakeResult (s -> v -> ((v, ()), text)) where
  makeResult s v = first (, ()) $ makeResult s v
instance MakeResult (s -> v -> (v, text)) => MakeResult (s -> v -> ((v, (), ()), text)) where
  makeResult s v = makeResult s v & \(x, y) -> ((x, (), ()), y)

instance IsChar Word8 where
  fromChar = c2w
  toChar = w2c

type ByteStringBuilder = Data.ByteString.Builder.Builder
type TextBuilder = Data.Text.Lazy.Builder.Builder

instance Eq ByteStringBuilder where x == y = Data.ByteString.Builder.toLazyByteString x == Data.ByteString.Builder.toLazyByteString y

shouldReturn :: (ShouldReturn m, HasCallStack, Show a, Eq a) => m a -> a -> Expectation
shouldReturn x y = shouldReturn' x y
class ShouldReturn m where shouldReturn' :: (HasCallStack, Show a, Eq a) => m a -> a -> Expectation
instance ShouldReturn IO where shouldReturn' x y = Test.Hspec.shouldReturn x y
instance Show a => ShouldReturn (Either a) where shouldReturn' x y = either (fail . ("Expected Right but received " <>) . show . Left @_ @()) (`shouldBe` y) x
instance ShouldReturn Maybe where shouldReturn' = maybe (const $ fail "Expected Just but received Nothing") shouldBe

instance IsList ByteStringBuilder where
  type Item ByteStringBuilder = Word8
  fromList = byteStringInsert . fromList
  fromListN n = byteStringThreshold n . fromList
  toList = toList . toLazyByteStringWith (safeStrategy smallChunkSize smallChunkSize) mempty

instance IsString () where fromString = const ()
instance IsString (Vector Char) where fromString = fromList
instance IsString a => IsString ((), a) where fromString = ((),) . fromString
--instance IsString Char where fromString [x] = x
--instance IsString Word8 where fromString [x] = c2w x

instance Show a => Show (IO a) where
  show x = unsafePerformIO $ x <&> (\y -> "IO " <> (bool id (cons '(' . flip snoc ')' ) $ ' ' `elem` y) y) . show

instance Eq a => Eq (IO a) where
  x == y = unsafePerformIO $ liftA2 (==) x y

class ShouldFail m where shouldFail :: Show a => m a -> Expectation
instance ShouldFail IO where shouldFail = (`shouldThrow` anyException)
instance ShouldFail Maybe where shouldFail = (`shouldSatisfy` isNothing)
--instance (Show a, Show b) => ShouldFail (Either a b) where shouldFail = (`shouldSatisfy` isLeft)

instance MonadState () IO where
  get = pure ()
  put = const $ pure ()
instance UpdateStateWithElement () char where updateStateWithElement = const id

