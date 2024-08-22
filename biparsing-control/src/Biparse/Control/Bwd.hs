module Biparse.Control.Bwd (Bwd(..)) where

newtype Bwd m u v = Bwd {runBwd :: u -> m v}
  deriving (Functor)
  deriving (Applicative, Alternative, Monad) via Kleisli m u
  deriving Profunctor via Kleisli m

instance (Default u, Show (m v)) => Show (Bwd m u v) where
  show (Bwd f) = "Bwd " <> show (f def)

instance (Default u, Eq (m v)) => Eq (Bwd m u v) where
  Bwd f == Bwd g = f def == g def

instance MonadFail m => MonadFail (Bwd m u) where
  fail = Bwd . const . fail

instance MonadError e m => MonadError e (Bwd m u) where
  throwError = Bwd . const . throwError
  catchError (Bwd x) f = Bwd \u -> catchError (x u) $ ($ u) . runBwd . f

--instance Monad m => Colift (Bwd m) m where
--  colift f = Bwd . (>=>) f . runBwd

instance (OneBwd a m, Functor m) => One a (Bwd m) where
  one = Bwd \x -> x <$ oneBwd x

instance Peek (Bwd m u) where peek = id
instance Try (Bwd m u) where try = id

type instance WhichDirection (Bwd _ _) = 'Backward
