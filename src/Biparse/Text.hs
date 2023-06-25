{-# LANGUAGE OverloadedStrings #-}
module Biparse.Text
  ( char
  , string
  , lines
  ) where

import Biparse.Biparser (Biparser, upon, one, SubElement, SubState, ElementContext, Const, SubStateContext, Iso)
import Biparse.General (stripPrefix, take)
import Biparse.List (splitOn)
import Data.InitTails (InitTails)

type CharElement c text = SubElement c text ~ Char

char :: forall c text m n u ss.
  ( CharElement c text
  , IsSequence ss
  , ElementContext c text
  , MonadState text m
  , MonadFail m
  , MonadWriter ss n
  , MonadFail n
  , ss ~ SubState c text
  )
  => Char
  -> Biparser c text m n u ()
char c = do
  c' <- one `upon` const c
  unless (c == c') $ fail $ "Did not find expected character " <> show c <> " and instead found " <> show c'

string :: forall c text m n u ss.
  ( CharElement c text
  , IsSequence ss
  , Show ss
  , MonadState text m
  , MonadFail m
  , MonadWriter ss n
  , SubStateContext c text
  , ss ~ SubState c text
  )
  => ss
  -> Const c text m n u
string = stripPrefix

lines :: forall c text m n ss.
  ( CharElement c text
  , IsSequence ss
  , MonadState text m
  , IsString ss
  , Show ss
  , MonadPlus m
  , MonadFail m
  , MonadFail n
  , MonadWriter ss n
  , Alternative n
  , SubStateContext c text
  , ElementContext c text
  , InitTails ss
  , ss ~ SubState c text
  )
  => Iso c m n text [ss]
lines = splitOn $ stripPrefix "\r\n" <|> take '\n'

