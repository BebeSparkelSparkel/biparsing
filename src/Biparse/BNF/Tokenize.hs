-- | Reference: https://www.w3.org/Notation.html
--
-- Does not accept windows '\r\n' newlines.

{-# LANGUAGE CPP #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}

{-# OPTIONS_GHC -ddump-types -ddump-cs-trace #-}

module Biparse.BNF.Tokenize
  ( Token'(..)
  , tokenize
  ) where

import Biparse.AlternativeAttributes (totalAtt, runAtt, addAtt, (<<|>>))
import Biparse.BNF.Tokenize.TH (take', count', constructorList)
import Biparse.BNF.Tokenize.Type (TokenT, Token, Token'(..))
import Biparse.BiparserT (Iso, SubElement, SubState, SubStateContext, ElementContext, uponM, mapMs)
import Biparse.General (FromNatural, takeWhile, take)
import Biparse.List (manyI)
import Control.Applicative hiding (many)
import Data.Functor.Identity (Identity(runIdentity))
import Data.Kind (Type)
import Data.Sequences (IsSequence, Index)
import Data.Type.Bool ()
import Prelude hiding (take, takeWhile)

type TokenConstructors :: [TokenT]
-- type TokenConstructors = '[ Name Type, Assignment, Alt, OP, CP, Count Type, Star, OB, CB, Pound, Comment Type, Spaces Type, Tabs Type, Newlines Type ]
type TokenConstructors = $(constructorList @(TokenT))

tokenize :: forall c text isoI isoMT t ts ss.
  ( Char ~ SubElement c text
  , IsSequence ss
  , SubStateContext c text
  , ElementContext c text
  , Eq ss
  , FromNatural (Index ss)
  , isoI ~ Iso c Identity Identity text t
  , isoMT ~ Iso c Maybe Maybe text t
  , t ~ Token ss
  , ts ~ TokenConstructors
  , ss ~ SubState c text
  ) => Iso c Identity Identity text [t]
tokenize = manyI $ mapMs (Just . runIdentity) (Just . runIdentity)
  $     $(take' '<' Less)
  -- <<|>> $(take' '>' Greater)
  -- <<|>> $(take' '|' Alt)
  -- <<|>> $(take' '(' OP)
  -- <<|>> $(take' ')' CP)
  -- <<|>> $(take' '*' Star)
  -- <<|>> $(take' '[' OB)
  -- <<|>> $(take' ']' CB)
  <<|>> $(take' '#' Pound)
  <<|>> addAtt @('Comment Type) comment
  -- <<|>> addAtt @Assignment (mkConst Assignment $ stripPrefix "::=")
  -- <<|>> addAtt @(Spaces Type) (Spaces <$> count ' ' `uponM` \case
  --   Spaces x -> pure x
  --   _ -> empty)
  <<|>> $(count' ' ' $ Spaces ())
  |>> addAtt @('Name Type) undefined
  where
  comment :: isoMT
  comment = Comment <$>
    (do
      take ';'
      takeWhile (/= '\n')
    ) `uponM` \case
      Comment x -> pure x
      _ -> empty
  -- (|>>) :: forall (implemented :: [TokenT]) (last :: TokenT). AA implemented isoMT -> AA '[last] (isoMT -> isoI) -> isoI
  x |>> y = runAtt @ts @_ @isoI $ totalAtt @_ @_ @isoMT @isoI x y

