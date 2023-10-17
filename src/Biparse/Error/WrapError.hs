{-# LANGUAGE AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Biparse.Error.WrapError
  ( wrapError
  , WrapError(..)
  ) where

import Control.Exception (IOException)

wrapError :: forall e s. WrapError e s => e -> s -> Error e s
wrapError e s = wrapError' @e @s e $ stateForError @e s

type WrapError :: Type -> Type -> Constraint
class WrapError e s where
  type Error e s :: Type
  type StateForError e s :: Type
  wrapError' :: e -> StateForError e s -> Error e s
  stateForError :: s -> StateForError e s

instance WrapError IOException s where
  type Error IOException s = IOException
  type StateForError IOException s = ()
  wrapError' = const
  stateForError = const ()

instance WrapError Void s where
  type Error Void s = Void
  type StateForError Void s = ()
  wrapError' = absurd
  stateForError = const ()

instance WrapError () s where
  type Error () s = ()
  type StateForError () s = ()
  wrapError' = const
  stateForError = const ()

instance MonadError Void Identity where
  throwError = absurd
  catchError = const

