{-# LANGUAGE OverloadedStrings #-}
module Biparse.Text
  ( CharElement
  , char
  , string
  , lines
  ) where

import Biparse.Biparser (Biparser, upon, one, SubElement, SubState, ElementContext, Const, SubStateContext, Iso)
import Biparse.General (stripPrefix)
import Biparse.List (splitOn)

type CharElement c s = SubElement c s ~ Char

char :: forall c s m n u text.
  ( CharElement c s
  , IsSequence text
  , ElementContext c s
  , MonadState s m
  , MonadFail m
  , MonadWriter text n
  , MonadFail n
  , text ~ SubState c s
  )
  => Char
  -> Biparser c s m n u ()
char c = do
  c' <- one `upon` const c
  unless (c == c') $ fail $ "Did not find expected character " <> show c <> " and instead found " <> show c'

string :: forall c s m n u text.
  ( CharElement c s
  , IsSequence text
  , Show text
  , MonadState s m
  , MonadFail m
  , MonadWriter text n
  , SubStateContext c s
  , text ~ SubState c s
  )
  => text
  -> Const c s m n u
string = stripPrefix

lines :: forall c s m n text.
  ( CharElement c s
  , IsSequence text
  , MonadState s m
  , IsString text
  , Show text
  , MonadPlus m
  , MonadFail m
  , MonadFail n
  , MonadWriter text n
  , Alternative n
  , SubStateContext c s
  , ElementContext c s
  , text ~ SubState c s
  )
  => Iso c m n s [text]
lines = splitOn $ string "\r\n" <|> char '\n'

