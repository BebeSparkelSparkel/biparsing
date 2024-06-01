{-# OPTIONS_GHC -Wno-missing-import-lists #-}
module Biparse.Mixes.Either 
  ( module Biparse.Mixes.Exports

  , BiparserEasy
  , IsoEasy
  , decodeEasy
  , encodeEasy
  , StringErrorIS

  , Biparser
  , pattern Biparser
  , Iso
  , FM
  , decode
  , encode

  , Mixes
  , Either(Left,Right)
  ) where

import Biparse.Mixes.Exports

import Biparse.Biparser qualified as BB
import Biparse.Biparser.StateReaderWriter qualified as SRW
import Control.Monad.ChangeMonad
import Control.Monad.EitherString
import Control.Monad.StateError
import Data.Either (Either(Left,Right))
import Data.Functor.Identity (Identity)

-- * Easy Types and Functions
--
-- | Try using these types and functions before using the more general ones in the next section.

type BiparserEasy c ss = Biparser c ss () ()
type IsoEasy c ss v = Iso c () () ss v
--type Unit c ss r w ws = Biparser css r w ws () ()
--type UnitEasy c ss r w ws = Biparser css r w ws () ()
--type Const c s m n r w ws u = 
--type ConstU c s m n r w ws u v = 

decodeEasy :: forall c ss u v.
  ( InitSuperState c ss
  , ChangeMonad StringErrorIS (Either (ErrorState String (SuperState c ss))) (Either String) ()
  )
  => BiparserEasy c ss u v
  -> SuperArg (SuperState c ss)
  -> ss
  -> Either String v
decodeEasy bp sa = changeMonad' @StringErrorIS () . SRW.evalForward bp . fromSubState @c sa

encodeEasy :: forall c ss u v.
  ( Monoid (AssociatedWriter ss)
  )
  => BiparserEasy c ss u v
  -> u
  -> Either String (AssociatedWriter ss)
encodeEasy bp u = runEitherString $ SRW.evalBackward bp () () () u

data StringErrorIS

--type instance ChangeFunction StringErrorIS _ _ = ()

instance ChangeMonad StringErrorIS (FM' (Identity ss)) (Either String) () where
  changeMonad' = const $ first _error
instance Show (Index ss) => ChangeMonad StringErrorIS (FM' (IndexPosition ss)) (Either String) () where
  changeMonad' = const $ first \(ErrorState msg (IndexPosition i _)) -> "At index " <> show i <> ": " <> msg
instance ChangeMonad StringErrorIS (FM' (Position FilePath ss)) (Either String) () where
  changeMonad' = const $ first \(ErrorState msg (Position fp l c _)) -> "In " <> fp <> " at line " <> show l <> " column " <> show c <> ": " <> msg

-- * More General Types and Functions
--
-- | Try using these before using the more general types and functions defined in Biparse.Biparser.StateReaderWriter and Biparse.Biparser

type Biparser c ss r ws = BiparserTemplate (FM c ss) BM c ss r ws
type Iso c r ws ss v = Biparser c ss r ws v v

type FM c ss = Either (ErrorState String (SuperState c ss))
type FM' s = Either (ErrorState String s)
type BM = EitherString

pattern Biparser :: ForwardMonad (Biparser c ss r ws u v) v -> (u -> BackwardMonad (Biparser c ss r ws u v) v) -> Biparser c ss r ws u v
pattern Biparser fw bw = BB.Biparser fw bw

decode :: forall c ss r ws u v.
  ( InitSuperState c ss
  )
  => Biparser c ss r ws u v
  -> SuperArg (SuperState c ss)
  -> ss
  -> Either (ErrorState String (SuperState c ss)) v
decode bp sa = SRW.evalForward bp . fromSubState @c sa

encode :: forall c ss r ws u v.
  ( Monoid (AssociatedWriter ss)
  )
  => Biparser c ss r ws u v
  -> r
  -> ws
  -> u
  -> Either String (AssociatedWriter ss)
encode bp r ws u = runEitherString $ SRW.evalBackward bp () r ws u

