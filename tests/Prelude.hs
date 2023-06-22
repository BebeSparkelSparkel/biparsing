module Prelude
  ( module GHC.Err
  , module System.IO
  , module System.IO.Error
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
  , module Data.Functor
  , module Control.Applicative
  , module Data.Bifunctor
  , module Data.Functor.Identity
  , module Data.Sequences
  , module Control.Monad.State
  , module Data.MonoTraversable
  , module Data.Tuple
  , module Biparse.Text.PositionContext

  , B.one
  , B.IdentityStateContext
  , B.peek
  , B.try
  , B.upon
  , B.split
  , B.isNull

  , Biparser
  , Iso
  , Unit
  , ConstU
  , runForward
  , evalForward
  , runBackward
  , fb
  , limit
  ) where

import Biparse.Text.PositionContext (LineColumn, Position(Position))
import Data.Sequences (drop, index)
import Data.Functor.Identity (Identity(Identity))
import Data.Maybe (Maybe(Just,Nothing), maybe)
import Data.Int (Int)
import Data.Monoid (mempty)
import Data.Ord ((>))
import Biparse.General
import Data.Eq ((==), (/=))
import Data.Text (Text)
import Data.Char (Char, isDigit)
import Data.Bool (Bool(True,False), otherwise)
import Data.List ((++))
import GHC.Err (undefined)
import Data.String (String, IsString(fromString))
import System.IO (IO, FilePath)
import System.IO.Error (isUserError)
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Test.QuickCheck.Instances.Text ()
import Text.Show (Show(show))
import Control.Monad ((>>=), return, (>>), fail)
import Data.Function ((.), ($), const)
import Control.Applicative (pure, (<|>), (<*), (*>), (<*>), empty, liftA2)
import Data.Functor (Functor(fmap), (<$>), ($>))
import Data.Bifunctor (first)
import Control.Monad.State (StateT(runStateT), get, put)
import Data.MonoTraversable (headMay)
import Data.Tuple (fst, snd)

import System.Timeout (timeout)
import Biparse.Biparser qualified as B
import Biparse.Biparser (backward, forward, SubState)
import Control.Monad.Writer (WriterT(runWriterT))

type Biparser c s m n u v = B.Biparser c s (StateT s m) (WriterT (SubState c s) n) u v 
type Iso c m n s v = Biparser c s m n v v
type Unit c s m n = Biparser c s m n () ()
type ConstU c s m n u v = Biparser c s m n u v

runForward :: forall c s m n u v. Biparser c s m n u v -> s -> m (v, s)
runForward = runStateT . forward

evalForward :: forall c s m n u v. Functor m => Biparser c s m n u v -> s -> m v
evalForward = (fmap fst .) . runForward

runBackward :: forall c s m n u v. Biparser c s m n u v -> u -> n (v, SubState c s)
runBackward = (runWriterT .) . backward

--fb :: Biparse c s m n u v -> ((s -> m (v,s)) ->
fb :: forall c s m n u v.
  String
  -> Biparser c s m n u v
  -> ((s -> m (v, s)) -> Spec)
  -> ((u -> n (v, SubState c s)) -> Spec)
  -> Spec
fb label bp fws bws = describe label do
  describe "forward" $ fws $ runForward bp
  describe "backward" $ bws $ runBackward bp

limit :: IO a -> IO a
limit = (>>= maybe (fail "Timeout") pure) . timeout 500

