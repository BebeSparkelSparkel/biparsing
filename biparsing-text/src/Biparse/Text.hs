{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedStrings #-}
module Biparse.Text
  ( char
  , string
  , stringShow
  ) where

import Biparse.General (stripPrefix, takeDi, Length, EqualityWrapper, StripPrefixEqualityCheck)

char :: forall p u char.
  ( Profunctor p
  , One char p
  , MonadFail (p u)
  , IsChar char
  , Show char
  , Eq char
  )
  => Char
  -> Const p u
char c = do
  let c' = fromChar @char c
  c'' <- one `uponConst` c'
  unless (c' == c'') $ fail $ "Did not find expected character " <> show c <> " and instead found " <> show c''

string :: forall p u text.
  ( Profunctor p
  , Try (p u)
  , BiN p
  , MonadFail (p u)
  , Length text
  , Show text
  , Applicative (EqualityWrapper (StripPrefixEqualityCheck p))
  , Eq (EqualityWrapper (StripPrefixEqualityCheck p) text)
  )
  => text
  -> Const p u
string = stripPrefix

-- | Tries matching the string @fromString $ show u@ when parsing.
-- Tries matching @u@ when printing.
stringShow :: forall p m u char.
  ( IsString char
  , Show char
  , Eq char
  , One char p
  , Try (p u)
  , MonadFail (p u)
  , Colift p m
  , MonadFail m
  , Eq u
  , Show u
  )
  => u
  -> Iso p u
stringShow u = takeDi (fromString $ show u) u

