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

type CharElement s char =
  ( IsChar char
  , Show char
  , Eq char
  , Ord char
  , SubElement s ~ char
  )

char :: forall c s m n u text char.
  ( IsSequence text
  , ElementContext c s
  , CharElement s char
  -- m
  , MonadState s m
  , MonadFail m
  , Alternative m
  -- n
  , MonadWriter text n
  , MonadFail n
  -- assignments
  , text ~ SubState s
  )
  => Char
  -> Biparser c s m n u ()
char c = do
  let c' = fromChar @char c
  c'' <- one `upon` const c'
  unless (c' == c'') $ fail $ "Did not find expected character " <> show c <> " and instead found " <> show c''

string :: forall c s m n u text.
  ( EqElement text
  , Show text
  , MonadState s m
  , MonadFail m
  , MonadWriter text n
  , SubStateContext c s
  , text ~ SubState s
  )
  => text
  -> Const c s m n u
string = stripPrefix

