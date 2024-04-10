{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE UndecidableInstances #-}
module Control.Monad.Trans.Writer.Selectable
  ( writer
  , writerT
  , runWriter
  , runWriterT
  , ContextualWriterTransformer
  , WriterTransformer
  , SelectableWriterTransformer(..)
  ) where

import Control.Applicative (Applicative(pure))
import Control.Monad.Writer.Class (MonadWriter)
import Control.Monad.Writer.Lazy qualified as L
import Control.Monad.Writer.Strict qualified as S
import Control.Monad.Trans.Writer.CPS qualified as CPS
import Data.Function ((.))
import Data.Functor.Identity (Identity(runIdentity))
import Data.Kind (Type)
import Data.Functor (Functor)
import Data.Monoid (Monoid)

writer :: forall context w m a m'. (ContextualWriterTransformer context w m m', Applicative m) => (a, w) -> WriterTransformer context w m a
writer = writerTransformer' . pure

writerT :: forall context w m a m'. ContextualWriterTransformer context w m m' => m (a, w) -> WriterTransformer context w m a
writerT = writerTransformer'

runWriter :: forall context w a m'. ContextualWriterTransformer context w Identity m' => WriterTransformer context w Identity a -> (a, w)
runWriter = runIdentity . runWriterTransformer'

runWriterT :: forall context w m a m'. ContextualWriterTransformer context w m m' => WriterTransformer context w m a -> m (a, w)
runWriterT = runWriterTransformer'

type ContextualWriterTransformer c w m m' =
  ( SelectableWriterTransformer (WriterTransformer c)
  , MonadWriter w m'
  , Functor m
  , m' ~ WriterTransformer c w m
  )

type WriterTransformer :: Type -> Type -> (Type -> Type) -> Type -> Type
type family WriterTransformer context

class SelectableWriterTransformer t where
  writerTransformer' :: (Functor m, Monoid w) => m (a, w) -> t w m a
  runWriterTransformer' :: forall w m a. Monoid w => t w m a -> m (a, w)

instance SelectableWriterTransformer L.WriterT where
  writerTransformer' = L.WriterT
  runWriterTransformer' = L.runWriterT

instance SelectableWriterTransformer S.WriterT where
  writerTransformer' = S.WriterT
  runWriterTransformer' = S.runWriterT

instance SelectableWriterTransformer CPS.WriterT where
  writerTransformer' = CPS.writerT
  runWriterTransformer' = CPS.runWriterT

