{-# LANGUAGE NoImplicitPrelude #-}
module Biparse.Text
  ( MonoParse
  , char
  --, string
  , lines
  ) where

import Biparse.BiparserT (BiparserT, upon, one)
import Biparse.List (splitElem)
import Control.Applicative (Alternative)
import Control.Monad (MonadPlus, unless, MonadFail(fail), (=<<))
import Control.Monad.Trans.State.Lazy (get, put)
import Control.Monad.Trans.Writer.Lazy (tell)
import Data.Char (Char)
import Data.Eq ((==))
import Data.Function (($), (.), const)
import Data.Maybe (maybe)
import Data.MonoTraversable (Element, MonoPointed)
import Data.Monoid (Monoid, (<>))
import Data.Sequences (IsSequence, stripPrefix)
import Data.String (String)
import Data.String.Conversions (ConvertibleStrings)
import Data.String.Conversions.Monomorphic (toString)
import Text.Show (show)

type MonoParse text m n =
  ( Element text ~ Char
  , Monoid text
  , MonoPointed text
  , IsSequence text

  , MonadFail m
  , MonadPlus m

  , Alternative n
  , MonadFail n
  )

char :: forall text m n u. MonoParse text m n => Char -> BiparserT text m n u ()
char c = do
  c' <- one `upon` const c
  unless (c == c') $ fail $ "Did not find expected character " <> show c <> " and instead found " <> show c'

--string :: forall text m n. (MonoParse text m n, ConvertibleStrings text String) => text -> BiparserT text m n text ()
--string prefix = BiparserT
--  ( maybe (fail $ "Did not find expected string '" <> toString prefix <> "'")
--          put
--  . stripPrefix prefix
--  =<< get
--  )
--  (const $ tell prefix)

lines :: MonoParse text m n => BiparserT text m n [text] [text]
lines = splitElem '\n'

