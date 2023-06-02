{-# LANGUAGE NoImplicitPrelude #-}
module Biparse.Text
  ( MonoParse
  , char
  --, string
  --, lines
  ) where

import Biparse.BiparserT (BiparserT, upon, one, SubElement, SubState, ElementContext, Const)
import Control.Applicative (Alternative)
import Control.Monad (MonadPlus, unless, MonadFail(fail))
import Data.Char (Char)
import Data.Eq ((==))
import Data.Function (($), const)
import Data.MonoTraversable (Element, MonoPointed)
import Data.Monoid (Monoid, (<>))
import Data.Sequences (IsSequence)
import Text.Show (show)
import Biparse.General (stripPrefix)

type CharElement c text = SubElement c text ~ Char

char :: forall c text m n u.
  ( MonoParse text m n
  , CharElement c text
  , IsSequence (SubState c text)
  , ElementContext c text
  )
  => Char
  -> BiparserT c text m n u ()
char c = do
  c' <- one `upon` const c
  unless (c == c') $ fail $ "Did not find expected character " <> show c <> " and instead found " <> show c'

string :: forall c text m n u.
  ( CharElement c s
  , IsSequence 
  , Show ss
  , MonadFail m
  , Monad n
  , ss ~ SubState c s
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

