{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleInstances #-}
module Prelude
  ( module Test.Prelude
  , module Biparse.Mixes.SubStates
  ) where

import Test.Prelude hiding (StrictText, StrictByteString, BuilderByteString)
import Biparse.Mixes.SubStates

import Language.Haskell.TH (Name, TypeQ, Lit, ExpQ, mkName, conT, Lit(StringL), litE)

instance IsString Name where fromString = mkName
instance IsString TypeQ where fromString = conT . mkName
instance IsString Lit where fromString = StringL
instance IsString ExpQ where fromString = litE . fromString

