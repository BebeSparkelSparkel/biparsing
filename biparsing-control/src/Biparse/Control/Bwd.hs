module Biparse.Control.Bwd (Bwd(..)) where

newtype Bwd m u v = Bwd {runBwd :: u -> m v} deriving (Functor)

instance (Default u, Show (m v)) => Show (Bwd m u v) where
  show (Bwd f) = "Bwd " <> show (f def)

instance (Default u, Eq (m v)) => Eq (Bwd m u v) where
  Bwd f == Bwd g = f def == g def

instance Applicative m => Applicative (Bwd m u) where
  pure = Bwd . const . pure
  Bwd x <*> Bwd y = Bwd \u -> x u <*> y u

instance Monad m => Monad (Bwd m u) where
  Bwd bw >>= f = Bwd \u -> bw u >>= ($ u) . runBwd . f

instance Alt m => Alt (Bwd m u) where
  Bwd x <!> Bwd y = Bwd \u -> x u <!> y u

instance MonadFail m => MonadFail (Bwd m u) where
  fail = Bwd . const . fail

instance MonadError e m => MonadError e (Bwd m u) where
  throwError = Bwd . const . throwError
  catchError (Bwd x) f = Bwd \u -> catchError (x u) $ ($ u) . runBwd . f

instance Functor m => Profunctor (Bwd m) where
  dimap f g (Bwd x) = Bwd $ fmap g . x . f

instance Monad m => Colift (Bwd m) m where
  colift f = Bwd . (>=>) f . runBwd

instance (OneBwd a m, Functor m) => One a (Bwd m) where
  one = Bwd \x -> x <$ oneBwd x

instance Peek (Bwd m u) where peek = id
instance Try (Bwd m u) where try = id

