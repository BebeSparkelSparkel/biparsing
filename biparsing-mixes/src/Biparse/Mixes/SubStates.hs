{-# LANGUAGE NoImplicitPrelude #-}
module Biparse.Mixes.SubStates
  ( module Data.ByteString
  , module Data.ByteString.Lazy
  , BuilderByteString
  , BuilderText
  , StrictText
  , LazyText
  ) where

import Data.ByteString (StrictByteString)
import Data.ByteString.Lazy (LazyByteString)
import Data.ByteString.Builder qualified as BB
import Data.Text qualified as TS
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Builder qualified as TB

type BuilderByteString = BB.Builder
type BuilderText = TB.Builder
type StrictText = TS.Text
type LazyText = TL.Text

