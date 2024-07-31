{-# OPTIONS_GHC -Wno-missing-import-lists #-}
module Biparse.Mixes.Either 
  ( module Biparse.Mixes.Exports

  , decode
  , encode

  , Either(Left,Right)
  ) where

import Biparse.Mixes.Exports

import Data.Either (Either(Left,Right))

decode :: forall s r text u v.
  ( Default s
  )
  => Biparser (Fwd (RWST r () (s, text) (Either (String, UnixLC)))) u v
  -> r
  -> text
  -> Either (String, UnixLC) v
decode bp r text = fmap fst3 $ runRWST (runFwd bp) r (def, text)

encode :: forall r w s u v.
  (
  )
  => Biparser (Bwd (RWST r w s (Either String))) u v
  -> r
  -> s
  -> u
  -> Either String w
encode bp r s u = fmap thd3 $ runRWST (runBwd bp u) r s

