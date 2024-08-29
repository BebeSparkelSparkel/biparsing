{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedStrings #-}
module Biparse.Text
  ( char
  , string
  , stringShow
  ) where

char :: forall p u char.
  ( Profunctor p
  , One char (p char)
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

string :: forall p u text. StripPrefix text (p u) => text -> Const p u
string = stripPrefix

-- | Tries matching the string @fromString $ show u@ when parsing.
-- Tries matching @u@ when printing.
stringShow :: forall text p m u.
  ( Try (p u)
  , MonadFail (p u)
  , ComapM p m
  , MonadFail m
  , Eq u
  , Show u
  , IsString text
  , Eq text
  , Show text
  , One text (p text)
  )
  => u
  -> Iso p u
stringShow u = takeDi (fromString @text $ show u) u

