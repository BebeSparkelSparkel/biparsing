{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedStrings #-}
module Biparse.Text
  ( CharElement
  , char
  , string
  --, lines
  --, lines'
  ) where

import Biparse.Biparser (Biparser, upon, one, SubElement, SubState, ElementContext, Const, SubStateContext)
import Biparse.General (stripPrefix)
import Data.Char (Char)

type CharElement c s char =
  ( IsChar char
  , Show char
  , Eq char
  , SubElement c s ~ char
  )

char :: forall c s m n u text char.
  ( IsSequence text
  , ElementContext c s
  , CharElement c s char
  -- m
  , MonadState s m
  , MonadFail m
  , Alternative m
  -- n
  , MonadWriter text n
  , MonadFail n
  -- assignments
  , text ~ SubState c s
  )
  => Char
  -> Biparser c s m n u ()
char c = do
  let c' = fromChar @char c
  c'' <- one `upon` const c'
  unless (c' == c'') $ fail $ "Did not find expected character " <> show c <> " and instead found " <> show c''

string :: forall c s m n u text char.
  ( CharElement c s char
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

--lines :: forall c s m n text.
--  ( CharElement c s
--  , IsSequence text
--  , MonadState s m
--  , IsString text
--  , Show text
--  , MonadPlus m
--  , MonadFail m
--  , MonadFail n
--  , MonadWriter text n
--  , Alternative n
--  , SubStateContext c s
--  , ElementContext c s
--  , text ~ SubState c s
--  )
--  => Iso c m n s [text]
--lines = splitWith $ string "\r\n" <|> char '\n'

