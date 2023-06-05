-- | Reference: https://www.w3.org/Notation.html
--
-- Does not accept windows '\r\n' newlines.

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PolyKinds #-}

{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
{-# OPTIONS_GHC -Wno-missing-import-lists #-}

module Biparse.BNF.Tokenize
  ( Token'(..)
  , tokenize
  ) where

import Biparse.AlternativeAttributes (AA, A, totalAtt, runAtt, (<|>>), a, emptyAtt)
import Biparse.BNF.Tokenize.TH (take', count', constructorList)
import Biparse.BNF.Tokenize.Type (TokenT, Token, Token'(..))
import Biparse.BiparserT (Iso, SubElement, SubState, SubStateContext, ElementContext, uponM, fixWith, mkConst, upon)
import Biparse.General (FromNatural, takeWhile, take, isNull, stripPrefix)
import Biparse.List (whileId)
import Control.Applicative (empty)
import Data.Char (isSpace)
import Data.Functor.Identity (Identity)
import Data.Kind (Type)
import Data.Sequences (IsSequence, Index)
import Data.String (IsString)
import Prelude hiding (take, takeWhile)

type TokenConstructors :: [TokenT]
type TokenConstructors = $(constructorList @(TokenT))

tokenize :: forall c text isoI isoMT t ts ss.
  ( Char ~ SubElement c text
  , IsSequence ss
  , SubStateContext c text
  , ElementContext c text
  , Eq ss
  , IsString ss
  , Show ss
  , FromNatural (Index ss)
  , isoI ~ Iso c Identity Identity text t
  , isoMT ~ Iso c Maybe Maybe text t
  , t ~ Token ss
  , ts ~ TokenConstructors
  , ss ~ SubState c text
  ) => Iso c Identity Identity text [t]
tokenize = whileId isNull
  $    $(take' '<' Less)
  <|>> $(take' '>' Greater)
  <|>> $(take' '|' Alt)
  <|>> $(take' '(' OP)
  <|>> $(take' ')' CP)
  <|>> $(take' '*' Star)
  <|>> $(take' '[' OB)
  <|>> $(take' ']' CB)
  <|>> $(take' '#' Pound)
  <|>> $(take' '"' DoubleQuote)
  <|>> a @('Comment Type) comment
  <|>> a @'Assignment (mkConst Assignment $ stripPrefix "::=")
  <|>> $(count' ' ' $ Spaces ())
  <|>> $(count' '\t' $ Tabs ())
  <|>> $(count' '\n' $ Newlines ())
  <|>> emptyAtt
  |>> a @('Name Type) (fixWith name)
  where
  infix 8 |>>
  (|>>) :: AA _ isoMT -> A _ (isoMT -> isoI) -> isoI
  x |>> y = runAtt @ts $ totalAtt x y
  
  comment :: isoMT
  comment = Comment <$>
    (do
      take ';'
      takeWhile (/= '\n')
    ) `uponM` \case
      Comment x -> pure x
      _ -> empty

  name :: isoI
  name = Name <$> takeWhile (not . isSpace) `upon` \case
    Name x -> x
    _ -> mempty

