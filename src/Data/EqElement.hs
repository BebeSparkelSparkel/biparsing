-- | Hack until https://github.com/snoyberg/mono-traversable/issues/216 is resolved
{-# LANGUAGE NoImplicitPrelude #-}
module Data.EqElement
  ( EqElement(..)
  ) where

import Data.Sequences (IsSequence)
import Data.Sequences qualified
import Data.MonoTraversable (Element)
import Data.Eq (Eq)
import Data.Maybe (Maybe)

import Data.List qualified
import Data.ByteString qualified
import Data.Text qualified
import Data.Sequence qualified

class (IsSequence seq, Eq (Element seq)) => EqElement seq where
  stripPrefix :: seq -> seq -> Maybe seq

instance Eq a => EqElement [a] where
  stripPrefix = Data.List.stripPrefix

instance EqElement Data.ByteString.ByteString where
  stripPrefix = Data.ByteString.stripPrefix

instance EqElement Data.Text.Text where
  stripPrefix = Data.Text.stripPrefix

instance Eq a => EqElement (Data.Sequence.Seq a) where
  stripPrefix = Data.Sequences.stripPrefix

