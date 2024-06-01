{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE UndecidableInstances #-}
module Biparse.FixFail {-# DEPRECATED "Should be put in a specifically pure backwards package" #-}
  ( FixFailM(..)
  , (!>>)
  ) where

import Control.Monad.State (StateT(StateT,runStateT))
import Control.Monad.Writer (WriterT(WriterT,runWriterT))
import Data.Function (($), flip)
import Data.Monoid (Monoid(mempty))

infixl 3 !>>
(!>>) :: FixFailM m m' => m a -> a -> m' a
(!>>) = flip fixFailM
class FixFailM m m' | m' -> m where fixFailM :: a -> m a -> m' a

instance FixFailM m m' => FixFailM (StateT s m) (StateT s m') where
  fixFailM x y = StateT \s -> (x,s) `fixFailM` runStateT y s
instance (FixFailM m m', Monoid w) => FixFailM (WriterT w m) (WriterT w m') where
  fixFailM x y = WriterT $ (x,mempty) `fixFailM` runWriterT y


instance (FixFailM m m', FixFailM n n') => FixFailM (Biparser c s m n u) (Biparser c s m' n' u) where
  fixFailM x (Biparser fw bw) = Biparser (fixFailM x fw) (fixFailM x . bw)
