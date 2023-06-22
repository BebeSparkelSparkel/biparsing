{-# LANGUAGE NoImplicitPrelude #-}
module Biparse.Text
  ( char
  , string
  , lines
  ) where

import Biparse.Biparser (Biparser, upon, one, SubElement, SubState, ElementContext, Const, SubStateContext, Iso)
import Biparse.General (stripPrefix, Take)
import Biparse.List (Many, splitElem)
import Control.Monad (unless, MonadFail(fail))
import Control.Monad.State.Class (MonadState)
import Control.Monad.Writer.Class (MonadWriter)
import Data.Char (Char)
import Data.Eq ((==))
import Data.Function (($), const)
import Data.Monoid ((<>))
import Data.Sequences (IsSequence)
import Text.Show (Show(show))

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
  ( Take c text m n
  , Many c text m n
  , CharElement c text
  , ss ~ SubState c text
  )
  => Iso c m n text [ss]
lines = splitElem '\n'

