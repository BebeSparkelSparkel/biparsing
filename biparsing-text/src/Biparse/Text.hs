{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedStrings #-}
module Biparse.Text
  ( CharElement
  , char
  , string
  , stringShow
  ) where

import Biparse.General (stripPrefix, Take, takeDi)

type CharElement s char =
  ( IsChar char
  , Show char
  , Eq char
  , Ord char
  , SubElement s ~ char
  )

char :: forall w c s m n u text char.
  ( IsSequence text
  , ElementContext c s
  , CharElement s char
  , ConvertElement c char w n
  -- m
  , MonadState s m
  , MonadFail m
  , Alt m
  -- n
  , MonadWriter w n
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

string :: forall c s m n u text w.
  -- m
  ( MonadState s m
  , MonadFail m
  -- n
  , MonadWriter w n
  -- text
  , EqElement text
  , Show text
  , ConvertSequence c text w n
  -- w
  -- context
  , SubStateContext c s
  -- assignments
  , ContextualStateTransformerPLEASEREMOVESUFFIX c text m
  , text ~ SubState s
  )
  => text
  -> Const c s m n u
string = stripPrefix

-- | Tries matching the string @fromString $ show u@ when parsing.
-- Tries matching @u@ when printing.
stringShow :: forall c s m n u text char w e.
  ( Take c s m n text char w e
  , Eq u
  , Show u
  , IsString char
  )
  => u
  -> Iso c m n s u
stringShow u = takeDi (fromString $ show u) u

