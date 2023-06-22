module Biparse.FixFail
  ( FixFail(..)
  , FixFailM(..)
  ) where

class FixFail m where fixFail :: a -> m a -> a

class FixFailM m where fixFailM :: a -> m a -> m a

instance FixFail Identity where fixFail _ = runIdentity
instance FixFail Maybe where fixFail = fromMaybe
instance FixFail (Either a) where fixFail = fromRight

instance (FixFail m, Applicative m) => FixFailM (StateT s m) where
  fixFailM x y = StateT $ \s -> pure $ (x,s) `fixFail` runStateT y s
instance (Monoid w, FixFail m, Applicative m) => FixFailM (WriterT w m) where
  fixFailM x y = WriterT $ pure $ (x,mempty) `fixFail` runWriterT y

