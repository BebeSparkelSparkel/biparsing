{-# LANGUAGE NoImplicitPrelude #-}
module Biparse.Mixes.SubStates
  ( module Data.ByteString
  , module Data.ByteString.Lazy
  , ByteStringBuilder
  , TextBuilder
  , TS.StrictText
  , TL.LazyText
  ) where

import Data.ByteString (StrictByteString)
import Data.ByteString.Lazy (LazyByteString)
import Data.ByteString.Builder qualified as BB
import Data.Text qualified as TS
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Builder qualified as TB

type ByteStringBuilder = BB.Builder
type TextBuilder = TB.Builder

