{-# LANGUAGE PolyKinds #-}
module Control.Monad.MonadProgenitor
  ( MonadProgenitor
  ) where

-- * Monad Progenitor for embedding error types

type MonadProgenitor :: k -> Type -> (Type -> Type)
type family MonadProgenitor progenitor state

