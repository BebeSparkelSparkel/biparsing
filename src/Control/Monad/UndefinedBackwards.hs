{-# OPTIONS_GHC -Wno-missing-methods #-}
module Control.Monad.UndefinedBackwards
  ( UndefinedBackwards
  ) where

data UndefinedBackwards w a

instance Functor (UndefinedBackwards w)
instance Applicative (UndefinedBackwards w)
instance Alternative (UndefinedBackwards w)
instance MonadPlus (UndefinedBackwards w)
instance MonadFail (UndefinedBackwards w)
instance Monad (UndefinedBackwards w)
instance Monoid w => MonadWriter w (UndefinedBackwards w)
