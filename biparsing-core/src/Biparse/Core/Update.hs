{-# LANGUAGE NoImplicitPrelude #-}
module Biparse.Core.Update (
UpdateStateWithSequence(..),
UpdateStateWithElement(..),
UpdateStateWithIndex(..),
) where

class UpdateStateWithSequence state subString where updateStateWithSequence :: subString -> state -> state

class UpdateStateWithElement state element where updateStateWithElement :: element -> state -> state

class UpdateStateWithIndex state index where updateStateWithIndex :: index -> state -> state
