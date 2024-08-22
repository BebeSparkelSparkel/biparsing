module Biparse.Control.Except (
Except(..),
except,
isException,
) where

newtype Except e a = Except {runExcept :: Either e a}
  deriving (Show, Eq, Functor, Applicative, Alternative, Monad)

except :: (e -> b) -> (a -> b) -> Except e a -> b
except f g = either f g . runExcept

isException :: Except e a -> Bool
isException = isLeft . runExcept

deriving instance MonadError e (Either e) => MonadError e (Except e)

instance IsString e => MonadFail (Except e) where
  fail = Except . Left . fromString  

instance Peek (Except e) where peek = id
instance Try (Except e) where try = id
instance OnError (Except e) where onError = const
