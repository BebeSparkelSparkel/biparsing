{-# LANGUAGE NoImplicitPrelude #-}
module Biparse.Core.Update (
UpdateStateWithSequence(..),
UpdateStateWithElement(..),
UpdateStateWithIndex(..),
) where

class UpdateStateWithSequence state subString where updateStateWithSequence :: subString -> state -> state
instance UpdateStateWithSequence () subString where updateStateWithSequence _ x = x

class UpdateStateWithElement state element where updateStateWithElement :: element -> state -> state
instance UpdateStateWithElement () element where updateStateWithElement _ x = x

class UpdateStateWithIndex state index where updateStateWithIndex :: index -> state -> state
instance UpdateStateWithIndex () index where updateStateWithIndex _ x = x
