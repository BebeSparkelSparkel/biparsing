{-# OPTIONS_GHC -Wno-missing-import-lists #-}
module Biparse.Mixes.Either 
  ( module Biparse.Biparser
  , module Biparse.General
  , module Biparse.List
  , module Biparse.Context.Index
  , module Biparse.Text
  , module Biparse.Text.Numeric
  , module Biparse.Text.LineBreak
  , module Biparse.Text.Context.LineColumn
  , module Biparse.Unordered
  , module Biparse.AssociatedWriter

  , BiparserEasy
  , IsoEasy
  , evalForwardEasy
  , evalBackwardEasy
  , StringErrorIS

  , Biparser
  , Iso
  , FM
  , evalForward
  , evalBackward

  , Mixes
  , Either(Left,Right)
  ) where

import Biparse.AssociatedWriter
import Biparse.Context.Index
import Biparse.General
import Biparse.List
import Biparse.Text
import Biparse.Text.Context.LineColumn
import Biparse.Text.LineBreak
import Biparse.Text.Numeric
import Biparse.Unordered

import Biparse.Biparser hiding (Biparser, Iso, Unit, Const, ConstU)
import Biparse.Biparser.StateReaderWriter qualified as SRW
import Control.Monad.ChangeMonad
import Control.Monad.EitherString
import Control.Monad.StateError
import Data.Either (Either(Left,Right))
import Data.Functor.Identity (Identity)

-- * Easy Types and Functions
--
-- | Try using these types and functions before using the more general ones in the next section.

type BiparserEasy c ss u v = Biparser c ss () () u v
type IsoEasy c ss v = Iso c () () ss v
--type Unit c ss r w ws = Biparser css r w ws () ()
--type UnitEasy c ss r w ws = Biparser css r w ws () ()
--type Const c s m n r w ws u = 
--type ConstU c s m n r w ws u v = 

evalForwardEasy :: forall c ss u v.
  ( InitSuperState c ss
  , ChangeMonad StringErrorIS (FM c ss) (Either String)
  )
  => BiparserEasy c ss u v
  -> SuperArg (SuperState c ss)
  -> ss
  -> Either String v
evalForwardEasy bp sa = SRW.evalForward @StringErrorIS bp . fromSubState @c sa

evalBackwardEasy :: forall c ss u v.
  ( Monoid (AssociatedWriter ss)
  )
  => BiparserEasy c ss u v
  -> u
  -> Either String (AssociatedWriter ss)
evalBackwardEasy bp u = runEitherString $ SRW.evalBackward bp () () u

data StringErrorIS

type instance ChangeFunction StringErrorIS _ _ = ()

instance ChangeMonad StringErrorIS (FM' (Identity ss)) (Either String) where
  changeMonad' = const $ first _error
instance Show (Index ss) => ChangeMonad StringErrorIS (FM' (IndexPosition ss)) (Either String) where
  changeMonad' = const $ first \(ErrorState msg (IndexPosition i _)) -> "At index " <> show i <> ": " <> msg
instance ChangeMonad StringErrorIS (FM' (Position FilePath ss)) (Either String) where
  changeMonad' = const $ first \(ErrorState msg (Position fp l c _)) -> "In " <> fp <> " at line " <> show l <> " column " <> show c <> ": " <> msg

instance ResultMonad (FM' s) StringErrorIS where
  type ResultingMonad (FM' s) StringErrorIS = Either String
  resultMonad = ()

-- * More General Types and Functions
--
-- | Try using these before using the more general types and functions defined in Biparse.Biparser.StateReaderWriter and Biparse.Biparser

type Biparser c ss r ws u v = SRW.Biparser (Mixes c) (SuperState c ss) (FM c ss) EitherString r (AssociatedWriter ss) ws u v
type Iso c r ws ss v = Biparser c ss r ws v v

type FM c ss = Either (Error c ss)
type FM' s = Either (ErrorState String s)
type Error c ss = ErrorState String (SuperState c ss)

evalForward :: forall c e ss r ws u v.
  ( InitSuperState c ss
  , ResultMonad (FM c ss) ()
  , ResultingMonad (FM c ss) () ~ Either e
  , ChangeMonad () (FM c ss) (Either e)
  )
  => Biparser c ss r ws u v
  -> SuperArg (SuperState c ss)
  -> ss
  -> Either e v
evalForward bp sa = SRW.evalForward @() bp . fromSubState @c sa

evalBackward :: forall c ss r ws u v.
  ( Monoid (AssociatedWriter ss)
  )
  => Biparser c ss r ws u v
  -> r
  -> ws
  -> u
  -> Either String (AssociatedWriter ss)
evalBackward bp r ws u = runEitherString $ SRW.evalBackward bp r ws u

