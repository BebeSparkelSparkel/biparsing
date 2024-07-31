{-# LANGUAGE DataKinds #-}
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
--fb,
RunForward(..),
MakeForwardResult(..),
RunBackward(..),
MakeBackwardResult(..),
EEP,
EESP,
errorPosition,
errorPosition',
EIP,
EISP,
errorIndex,
--FM,
--FMIO,
--BM,
--BM',
BMIO,
BMIO',
ByteStringBuilder,
TextBuilder,
ShouldReturn,
shouldReturn,
ShouldFail(..),
) where

import Type.Reflection as Export (Typeable, typeRep)
--import Control.Monad.EitherString as Export (EitherString, pattern EString, pattern EValue, isString)
import Biparse.Comap as Export
import Biparse.Control.Bwd as Export
import Biparse.Control.Fwd as Export
import Biparse.Core.Aliases as Export
import Biparse.Core.Classes as Export
import Biparse.General as Export
import Biparse.State.Index as Export (IndexPosition(IndexPosition))
import Biparse.Text.State.LineColumn as Export
import Control.Applicative as Export (Applicative(pure,(<*>)), (*>), (<*), liftA2, empty)
import Control.Monad as Export (Monad((>>=),return), when, sequence)
import Control.Monad.Error.Class as Export (throwError, catchError)
import Control.Monad.Fail as Export (MonadFail(fail))
import Control.Monad.IO.Class as Export (liftIO)
import Control.Monad.State.Class as Export (get, put)
import Control.Monad.Trans.RWS.CPS as Export (RWST, mapRWST, rwsT, runRWST)
import Control.Monad.Writer.Class as Export (MonadWriter(tell))
import Control.Monad.Writer as Export (WriterT)
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
import Data.MonoTraversable as Export (olength)
import Data.Monoid as Export (Monoid(mempty))
import Data.Ord as Export
import Data.Profunctor as Export (Profunctor)
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
import Biparse.Control.StateError as Export (StateErrorT)
import Control.Monad.State as Export (StateT)
import Biparse.Control.File as Export (FileT, UpdateState(runUpdateState), OpenFile(openFile))
import Data.Proxy as Export

-- Internal Imports
import Biparse.Control.File (runFileT, OpenFrom)
import Biparse.State.Lenses (HasDataId)
import Biparse.Control.StateError (runStateErrorT)
import Control.Monad.State (runStateT)
import Control.Monad.Writer (runWriterT)
import Data.ByteString.Builder qualified
import Data.ByteString.Builder.Internal (byteStringInsert, byteStringThreshold, toLazyByteStringWith, safeStrategy, smallChunkSize)
import Data.Either (Either(Left))
import Data.List (elem)
--import Data.List (foldr)
import Data.MonoTraversable (Element, MonoPointed(opoint))
import Data.Sequences (cons, snoc)
import Data.Text.Lazy.Builder qualified
import GHC.Exts (IsList(..))
import System.IO.Unsafe (unsafePerformIO)
import Test.Hspec qualified
--import Test.Hspec.Core.Spec (SpecM)
import System.IO.Temp (withSystemTempFile)
import System.IO (openFile, openTempFile, hSeek, hGetContents, SeekMode(AbsoluteSeek), IOMode(ReadMode, WriteMode), withFile, hClose)
import System.IO qualified

--combinations :: [a] -> [b] -> [(a,b)]
--combinations xs = foldr (\y zs -> fmap (,y) xs <> zs) [] 
--
--data HeteroList a where
--  Nil :: HeteroList '[]
--  (:^) :: a -> HeteroList b -> HeteroList (a ': b)
--
--class Concat a b where
--  type (++) a b
--  (++) :: HeteroList a -> HeteroList b -> HeteroList (a ++ b)
--instance Concat (a ': b) c where
--  type (a ': b) ++ c = a ': (b ++ c)
--  (x :^ xs) ++ ys = x :^ (xs ++ ys)
--instance Concat '[] a where
--  type '[] ++ a = a
--  Nil ++ xs = xs
--
--class Traverse 
--   :: (forall
--
--
--type Forwards =
--  , FM   UnixLC    String
--  , FM   WindowsLC String
--  , FMIO UnixLC    String
--  , FMIO WindowsLC String
--  , FM   UnixLC    String
--  , FM   WindowsLC String
--  , FMIO UnixLC    String
--  , FMIO WindowsLC String
--  )
--type Backward =
--  ( 
--  )

--data Direction a
--  = Forward  a
--  | Backward a
--type PickProfunctor :: Direction (Type -> Type -> Type, Type) -> Type -> Type -> Type
--type family PickProfunctor a where
--  PickProfunctor (Forward  '(p,_)) = p
--  PickProfunctor (Backward '(p,_)) = p
--type ForwardPick :: Direction (Type -> Type -> Type, Type) -> Type -> Type
--type family ForwardPick a b where
--  ForwardPick (Forward  '(_, a)) _ = a -> Spec
--  ForwardPick (Backward _) a = a -> Spec
--type BackwardPick :: Direction (Type -> Type -> Type, Type) -> Type -> Type
--type family BackwardPick a b
--
--class PickForward (d :: Direction (Type -> Type -> Type, Type)) a b | d b -> a where pickForward  :: a -> b
--instance PickForward (Forward k) Spec Spec where pickForward  = id
--instance PickForward (Backward k) Spec Spec where pickForward  = const $ pure ()
--
--class PickBackward (d :: Direction (Type -> Type -> Type, Type)) a b | d b -> a where pickBackward :: a -> b
--instance PickBackward (Forward k) Spec Spec where pickBackward = const $ pure ()
--instance PickBackward (Backward k) Spec Spec where pickBackward = id
--
----instance PickDirection (Forward (p, )) (Biparser (PickProfunctor d) u v) b0 (ba -> ForwardPick d a0) where
----instance PickDirection d (Biparser (PickProfunctor d) u v) b0 (ba -> ForwardPick d a0) where
----instance PickDirection d (Biparser (PickProfunctor d) u v) b0 (ba -> BackwardPick d a0) where
--
--type SpecDirected :: Direction (Type -> Type -> Type, Type) -> Type
--type SpecDirected d = SpecDirectedM d () ()
--type SpecDirectedM :: Direction (Type -> Type -> Type, Type) -> Type -> Type -> Type
--newtype SpecDirectedM d a r = SpecDirectedM (SpecM a r)
--  deriving (Functor, Applicative, Monad)
--
----fb :: forall (d :: Direction Type) a a' b u v (p :: Type -> Type -> Type) (q :: Type -> Type -> Type).
----  ( RunForward p a b
----  , p ~ ForwardPick d
----  , RunBackward q a'
----  , q ~ BackwardPick d
----  , forall z. PickDirection d Spec (SpecM () z) Spec
----  )
----  => String
----  -> Biparser (PickProfunctor d) u v
----  -> a
----  -> a'
----  -> ((b -> ForwardResult (ForwardPick d) v) -> Spec)
----  -> ((u -> BackwardResult (BackwardPick d) v) -> Spec)
----  -> SpecDirected d
----fb description bp a a' fws bws = SpecDirectedM $ describe description do
----  pickForward   @d $ describe "forward"  $ fws $ runForward  (f  :: Biparser p u v) a
----  pickBackward @d $ describe "backward" $ bws $ runBackward (bp' :: Biparser q u v) a'
----  where
----  f = pickForward bp
----  b = pickBackward bp
--
--fb :: forall (d :: Direction (Type -> Type -> Type, Type)) fa fa' ba u v bp a p.
--  ( PickForward d Spec Spec
--  , PickForward d bp (fa -> fa' -> ForwardResult p v)
--  , ForwardPick d a ~ (fa' -> ForwardResult p v)
--  , PickBackward d Spec Spec
--  , PickBackward d bp (ba -> u -> BackwardResult p v)
--  , BackwardPick d a ~ (u -> BackwardResult p v)
--  , bp ~ Biparser p u v
--  , p ~ PickProfunctor d
--  )
--  => String
--  -> Biparser (PickProfunctor d) u v
--  -> fa
--  -> ba
--  -> (ForwardPick  d a -> Spec)
--  -> (BackwardPick d a -> Spec)
--  -> SpecDirected d
--fb description bp forwardArg backwardArg forwardTests backwardTests =
--  SpecDirectedM $ describe description do
--    pickForward  @d $ describe "forward"  $ forwardTests  $ pickForward  @d bp forwardArg :: Spec
--    pickBackward @d $ describe "backward" $ backwardTests $ pickBackward @d bp backwardArg :: Spec
--
--test ::
--  ( ForwardPick d a ~ (String -> m (Char, (Position c (), String)))
--  ) => SpecDirected d
--test = fb "test" one () ()
--  (\f -> it "test" $ f "abc" `shouldReturn` ('a', (Position () 1 2, "bc")))
--  \b -> it "test" $ b 'a' `shouldReturn` ('a', "a")

newtype Swapped b a = Swapped (a,b) deriving (Show, Eq)

class RunForward p a b m c | p -> m c where
  runForward :: Biparser p u v -> a -> b -> m (c v)
instance (Default s, IsString seq) => RunForward (Fwd (StateT (s,seq) IO)) a String IO (Swapped (s,seq)) where
  runForward x _ str = Swapped <$> runStateT (runFwd x) (def, fromString str)
instance (Default s, IsString seq) => RunForward (Fwd (StateErrorT (s,seq) (Either ((s,seq),e)))) a String (Either ((s,seq),e)) (Swapped (s,seq)) where
  runForward x _ str = Swapped <$> runStateErrorT (runFwd x) (def, fromString str)
instance OpenFrom text => RunForward (Fwd (FileT text IO)) FilePath String IO Identity where
  runForward x fp str = withSystemTempFile fp \fp' h -> do
    System.IO.hPutStr h str
    hClose h
    Identity <$> runFileT (runFwd x) fp' ReadMode
instance (Default s', HasDataId s' s () FilePath, OpenFrom text) => RunForward (Fwd (FileT text (UpdateState (StateT s IO)))) FilePath String IO (Swapped s) where
  runForward x fp str = withSystemTempFile fp \fp' h -> do
    System.IO.hPutStr h str
    hClose h
    Swapped <$> runStateT (runUpdateState $ runFileT (runFwd x) fp' ReadMode) (def @s' & dataId .~ fp)

class MakeForwardResult a where makeForwardResult :: a
instance MakeForwardResult (Position () FilePath -> IndexPosition FilePath -> String -> v -> Identity v) where
  makeForwardResult _ _ _ x = Identity x
instance IsString text => MakeForwardResult (Position () FilePath -> IndexPosition FilePath -> String -> v -> Swapped (Position context (), text) v) where
  makeForwardResult p _ s x = Swapped (x, (coerce $ p & dataId .~ (), fromString s))
instance MakeForwardResult (Position () FilePath -> IndexPosition FilePath -> String -> v -> Prelude.Swapped (Position context FilePath) v) where
  makeForwardResult p _ _ x = Swapped (x, coerce p)

class RunBackward p a m c | p -> m c where
  runBackward :: Biparser p u v -> a -> u -> m (c v)
instance RunBackward (Bwd (WriterT w IO)) FilePath IO (Swapped w) where
  runBackward x _ u = Swapped <$> runWriterT (runBwd x u)
instance RunBackward (Bwd (WriterT w (Either String))) FilePath (Either String) (Swapped w) where
  runBackward x _ u = Swapped <$> runWriterT (runBwd x u)
instance (IsString text, OpenFrom text) => RunBackward (Bwd (FileT text IO)) FilePath IO (Swapped text) where
  runBackward x fp u = withSystemTempFile fp \fp' h -> do
    hClose h
    y <- runFileT (runBwd x u) fp' WriteMode 
    text <- System.IO.readFile fp'
    return $ Swapped (y, fromString text)
instance (IsString text, OpenFrom text, Default s) => RunBackward (Bwd (FileT text (StateT s IO))) FilePath IO (Swapped (s,text)) where
  runBackward x fp u = withSystemTempFile fp \fp' h -> do
    hClose h
    (y, s) <- runStateT (runFileT (runBwd x u) fp' WriteMode) def
    text <- System.IO.readFile fp'
    return $ Swapped (y, (s, fromString text))

class MakeBackwardResult a where makeBackwardResult :: a
instance IsString text => MakeBackwardResult (String -> v -> Swapped text v) where
  makeBackwardResult str v = Swapped (v, fromString str)
--instance (Default s, IsString text) => MakeBackwardResult (String -> v -> Swapped (s, text) v) where
--  makeBackwardResult str v = Swapped (v, (def, fromString str))

type EEP context e text = Either ((Position context (), text), e)
type EESP context text = EEP context String text

errorPosition :: Eq e => Int -> Int -> EEP context e text b -> Bool
errorPosition = errorPosition' Nothing

errorPosition' :: Eq e => Maybe e -> Int -> Int -> EEP context e text b -> Bool
errorPosition' me l c = \case
  Left ((Position _ l' c', _), e) -> l == l' && c == c' && maybe True (== e) me
  _ -> False

type EIP e ss = Either (IndexPosition ss, e)
type EISP ss = EIP String ss

errorIndex :: Int -> Either (IndexPosition ss, e) b -> Bool
errorIndex i = \case
  Left (IndexPosition _ i', _) -> i == i'
  _ -> False

--type FM context text = Fwd (StateErrorT (Position context (), text) (EESP context text))
--instance RunForward (FM context text) () text where
--  type ForwardResult (FM context text) v = EESP context text (v, (Position context (), text))
--  runForward x _ text = runStateErrorT (runFwd x) (def, text)
--
--type FMIO context text = Fwd (StateT (Position context (), text) IO)
--instance RunForward (FMIO context text) () text where
--  type ForwardResult (FMIO context text) v = IO (v, (Position context (), text))
--  runForward x _ text = runStateT (runFwd x) (def, text)

type instance Item' (StateErrorT (_,text) _) = Element text
type instance Item' (WriterT w _) = Element w

--type BM text = Bwd (WriterErrorT text (Either String))
--instance Monoid text => RunBackward (BM text) () where
--  type BackwardResult (BM text) v = Either String (v, text)
--  runBackward x _ u = runWriterErrorT (runBwd x u)

--type BM' r s text = Bwd (RWST r text s (Either String))
--instance Monoid text => RunBackward (BM' r s text) (r, s) where
--  type BackwardResult (BM' r s text) v = Either String (v, s, text)
--  runBackward x (r, s) u = runRWST (runBwd x u) r s

type BMIO text = Bwd (WriterT text IO)
--instance RunBackward (BMIO text) () where
--  type BackwardResult (BMIO text) v = IO (v, text)
--  runBackward x _ u = runWriterT (runBwd x u)

type BMIO' r s text = Bwd (RWST r text s IO)
--instance Monoid text => RunBackward (BMIO' r s text) (r, s) where
--  type BackwardResult (BMIO' r s text) v = IO (v, s, text)
--  runBackward x (r, s) u = runRWST (runBwd x u) r s

instance
  ( MonoPointed text
  , Monoid text
  , Element text ~ char
  , Monad m
  ) => OneBwd char (WriterT text m) where
  oneBwd = tell . opoint

instance IsChar Word8 where
  fromChar = c2w
  toChar = w2c

--instance IsList a => IsList (Identity a) where
--  type Item (Identity a) = Item a
--  fromList = Identity . fromList
--  toList = GHC.Exts.toList . runIdentity

type ByteStringBuilder = Data.ByteString.Builder.Builder
type TextBuilder = Data.Text.Lazy.Builder.Builder

instance Eq ByteStringBuilder where x == y = Data.ByteString.Builder.toLazyByteString x == Data.ByteString.Builder.toLazyByteString y

shouldReturn :: (ShouldReturn m, HasCallStack, Show a, Eq a) => m a -> a -> Expectation
shouldReturn x y = shouldReturn' x y
class ShouldReturn m where shouldReturn' :: (HasCallStack, Show a, Eq a) => m a -> a -> Expectation
instance ShouldReturn IO where shouldReturn' x y = Test.Hspec.shouldReturn x y
instance Show a => ShouldReturn (Either a) where shouldReturn' x y = either (fail . ("Expected Right but received " <>) . show . Left @_ @()) (`shouldBe` y) x

instance IsList ByteStringBuilder where
  type Item ByteStringBuilder = Word8
  fromList = byteStringInsert . fromList
  fromListN n = byteStringThreshold n . fromList
  toList = toList . toLazyByteStringWith (safeStrategy smallChunkSize smallChunkSize) mempty

instance IsString (Vector Char) where fromString = fromList
instance IsString a => IsString ((), a) where fromString = ((),) . fromString

instance Show a => Show (IO a) where
  show x = unsafePerformIO $ x <&> (\y -> "IO " <> (bool id (cons '(' . flip snoc ')' ) $ ' ' `elem` y) y) . show

instance Eq a => Eq (IO a) where
  x == y = unsafePerformIO $ liftA2 (==) x y

class ShouldFail a where shouldFail :: a -> Expectation
instance ShouldFail (IO a) where shouldFail = (`shouldThrow` anyException)
instance (Show a, Show b) => ShouldFail (Either a b) where shouldFail = (`shouldSatisfy` isLeft)

