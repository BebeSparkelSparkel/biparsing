{-# OPTIONS_GHC -Wno-missing-import-lists #-}
module Biparse.Mixes.IO 
  ( module Biparse.Mixes.Exports

  , BiparserEasy
  , IsoEasy
  , decodeEasy
  , encodeEasy

  , Biparser
  , pattern Biparser
  , Iso
  , decode
  , encode

  , Mixes
  , IO
  ) where

import Biparse.Mixes.Exports

import Biparse.Biparser qualified
import Biparse.Biparser.StateReaderWriter qualified as SRW
import System.IO (IO)

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
  )
  => BiparserEasy c ss u v
  -> SuperArg (SuperState c ss)
  -> ss
  -> IO v
decodeEasy bp sa = SRW.evalForward bp . fromSubState @c sa

encodeEasy :: forall c ss u v.
  ( Monoid (AssociatedWriter ss)
  )
  => BiparserEasy c ss u v
  -> u
  -> IO (AssociatedWriter ss)
encodeEasy bp u = SRW.evalBackward bp () () u

-- * More General Types and Functions
--
-- | Try using these before using the more general types and functions defined in Biparse.Biparser.StateReaderWriter and Biparse.Biparser

type Biparser c ss r ws = SRW.Biparser (Mixes c) (SuperState c ss) IO IO r (AssociatedWriter ss) ws
type Iso c r ws ss v = Biparser c ss r ws v v

pattern Biparser :: ForwardMonad (Biparser c ss r ws u v) v -> (u -> BackwardMonad (Biparser c ss r ws u v) v) -> Biparser c ss r ws u v
pattern Biparser fw bw = Biparse.Biparser.Biparser fw bw

decode :: forall c ss r ws u v.
  ( InitSuperState c ss
  )
  => Biparser c ss r ws u v
  -> SuperArg (SuperState c ss)
  -> ss
  -> IO v
decode bp sa = SRW.evalForward bp . fromSubState @c sa

encode :: forall c ss r ws u v.
  ( Monoid (AssociatedWriter ss)
  )
  => Biparser c ss r ws u v
  -> r
  -> ws
  -> u
  -> IO (AssociatedWriter ss)
encode bp r ws u = SRW.evalBackward bp r ws u

