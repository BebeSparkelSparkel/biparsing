{-# OPTIONS_GHC -Wno-orphans #-}
module Biparse.Error.WrapError
  ( WrapError(..)
  ) where

import Control.Exception (IOException)
import Control.Monad.Except (catchError)

class WrapError e s where
  type Error e s :: Type
  wrapError :: e -> s -> Error e s

instance WrapError IOException s where
  type Error IOException s = IOException
  wrapError = const

instance WrapError Void s where
  type Error Void s = Void
  wrapError = absurd

instance WrapError () s where
  type Error () s = ()
  wrapError = const

instance MonadError Void Identity where
  throwError = absurd
  catchError = const

