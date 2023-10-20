{-# LANGUAGE AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Biparse.Error.WrapError
  ( wrapError
  , WrapError(..)
  ) where

import Control.Exception (IOException)

wrapError :: forall e s er. WrapError e s er => e -> s -> er
wrapError e s = wrapError' @e @s e $ stateForError @e @_ @er s

type WrapError :: Type -> Type -> Type -> Constraint
class WrapError e s er where
  type StateForError e s er :: Type
  wrapError' :: e -> StateForError e s er -> er
  stateForError :: s -> StateForError e s er

instance WrapError IOException s IOException where
  type StateForError IOException s IOException = ()
  wrapError' = const
  stateForError = const ()

instance WrapError Void s Void where
  type StateForError Void s Void = ()
  wrapError' = absurd
  stateForError = const ()

instance WrapError () s () where
  type StateForError () s () = ()
  wrapError' = const
  stateForError = const ()

instance MonadError Void Identity where
  throwError = absurd
  catchError = const

