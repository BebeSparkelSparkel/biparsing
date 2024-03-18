{-# LANGUAGE AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -Wno-missing-import-lists #-}
module Biparse.Easy.Either 
  ( module Biparse.Biparser
  , module Biparse.General
  , module Biparse.List
  , module Biparse.Context.Index
  , module Biparse.Text
  , module Biparse.Text.Numeric
  , module Biparse.Text.LineBreak
  , module Biparse.Text.Context.LineColumn
  , module Biparse.Unordered

  , BiparserEasy
  , IsoEasy
  , evalForwardEasy
  , evalBackwardEasy
  
  , Biparser
  , Iso
  , evalForward
  , evalBackward

  , Either(..)
  ) where

import Biparse.Biparser hiding (Biparser, Iso, Unit, Const, ConstU)
import Biparse.Biparser.StateReaderWriter qualified as SRW
import Control.Monad.StateError
import Control.Monad.ChangeMonad
import Control.Monad.EitherString

import Biparse.General
import Biparse.List
import Biparse.Context.Index
import Biparse.Text
import Biparse.Text.Numeric
import Biparse.Text.LineBreak
import Biparse.Text.Context.LineColumn
import Biparse.Unordered

-- * Easy Types and Functions
--
-- | Try using these types and functions before using the more general ones in the next section.

type BiparserEasy c ss u v = Biparser c ss () ss () u v
type IsoEasy c ss v = Iso c () ss () ss v
--type Unit c ss r w ws = Biparser css r w ws () ()
--type UnitEasy c ss r w ws = Biparser css r w ws () ()
--type Const c s m n r w ws u = 
--type ConstU c s m n r w ws u v = 

evalForwardEasy :: forall c ss u v.
  ( InitSuperState c ss
  , ChangeMonad StringError (FM c ss) (Either String)
  )
  => BiparserEasy c ss u v
  -> ss
  -> Either String v
evalForwardEasy bp = SRW.evalForward @StringError bp . fromSubState @c

evalBackwardEasy :: forall c ss u v.
  ( SRW.BackwardC c EitherString ss
  )
  => BiparserEasy c ss u v
  -> u
  -> Either String ss
evalBackwardEasy bp u = runEitherString $ SRW.evalBackward bp () () u

data StringError

type instance ChangeFunction StringError _ _ = ()

instance ChangeMonad StringError (FM' (Identity ss)) (Either String) where
  changeMonad' = const $ first _error

instance ResultMonad (FM' s) StringError where
  type ResultingMonad (FM' s) StringError = Either String
  resultMonad = ()

-- * More General Types and Functions
--
-- | Try using these before using the more general types and functions defined in Biparse.Biparser.StateReaderWriter and Biparse.Biparser

type Biparser c ss r w ws u v = SRW.Biparser c (SuperState c ss) (FM c ss) EitherString r w ws u v
type Iso c r w ws ss v = Biparser c ss r w ws v v
type FM c ss = Either (Error c ss)
type FM' s = Either (ErrorState String s)
type Error c ss = ErrorState String (SuperState c ss)

evalForward :: forall c e ss r w ws u v.
  ( InitSuperState c ss
  , ResultMonad (FM c ss) ()
  , ResultingMonad (FM c ss) () ~ Either e
  , ChangeMonad () (FM c ss) (Either e)
  )
  => Biparser c ss r w ws u v
  -> ss
  -> Either e v
evalForward bp = SRW.evalForward @() bp . fromSubState @c

evalBackward :: forall c ss r w ws u v.
  ( SRW.BackwardC c EitherString w
  )
  => Biparser c ss r w ws u v
  -> r
  -> ws
  -> u
  -> Either String w
evalBackward bp r ws u = runEitherString $ SRW.evalBackward bp r ws u

