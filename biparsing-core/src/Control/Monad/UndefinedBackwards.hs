{-# OPTIONS_GHC -Wno-missing-methods #-}
module Control.Monad.UndefinedBackwards
  ( UndefinedBackwards
  , returnAbsurd
  ) where

import GHC.Err (undefined)
import Control.Monad.ChangeMonad (ChangeMonad, changeMonad')
import Control.Monad (MonadPlus)
import Control.Applicative (Alternative)

data UndefinedBackwards w a

returnAbsurd :: UndefinedBackwards w a
returnAbsurd = undefined

instance Functor (UndefinedBackwards w)
instance Applicative (UndefinedBackwards w)
instance Alternative (UndefinedBackwards w)
instance MonadPlus (UndefinedBackwards w)
instance MonadFail (UndefinedBackwards w)
instance Monad (UndefinedBackwards w)
instance Monoid w => MonadWriter w (UndefinedBackwards w)

instance ChangeMonad is (UndefinedBackwards w) (UndefinedBackwards w') () where
  changeMonad' = const $ const returnAbsurd

instance MonadTrans t' => ChangeMonad is (t (UndefinedBackwards w)) (t' (UndefinedBackwards w')) () where
  changeMonad' = const $ const $ lift returnAbsurd

