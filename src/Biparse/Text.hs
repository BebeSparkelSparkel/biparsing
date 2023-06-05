{-# LANGUAGE NoImplicitPrelude #-}
module Biparse.Text
  ( char
  , string
  --, lines
  ) where

import Biparse.BiparserT (BiparserT, upon, one, SubElement, SubState, ElementContext, Const, SubStateContext)
import Control.Monad (unless, MonadFail(fail))
import Data.Char (Char)
import Data.Eq ((==))
import Data.Function (($), const)
import Data.Monoid ((<>))
import Data.Sequences (IsSequence)
import Text.Show (Show(show))
import Biparse.General (stripPrefix)

type CharElement c text = SubElement c text ~ Char

char :: forall c text m n u.
  ( CharElement c text
  , IsSequence (SubState c text)
  , ElementContext c text
  , MonadFail m
  , MonadFail n
  )
  => Char
  -> BiparserT c text m n u ()
char c = do
  c' <- one `upon` const c
  unless (c == c') $ fail $ "Did not find expected character " <> show c <> " and instead found " <> show c'

string :: forall c text m n u ss.
  ( CharElement c text
  , IsSequence ss
  , Show ss
  , MonadFail m
  , MonadFail n
  , SubStateContext c text
  , ss ~ SubState c text
  )
  => ss
  -> Const c text m n u
string = stripPrefix

--string :: forall text m n. (MonoParse text m n, ConvertibleStrings text String) => text -> BiparserT c text m n text ()
--string prefix = BiparserT
--  ( maybe (fail $ "Did not find expected string '" <> toString prefix <> "'")
--          put
--  . stripPrefix prefix
--  =<< get
--  )
--  (const $ tell prefix)

--lines :: forall c text m n. MonoParse text m n => BiparserT c text m n [text] [text]
--lines = splitElem '\n'

