module Biparse.Control.Except (
Except(..),
except,
isException,
) where

import Biparse.Control.Bwd (Bwd(Bwd))
import Biparse.Control.Fwd (Fwd(Fwd))

newtype Except e a = Except {runExcept :: Either e a}
  deriving (Show, Eq, Functor, Applicative, Alternative, Monad)

except :: (e -> b) -> (a -> b) -> Except e a -> b
except f g = either f g . runExcept

isException :: Except e a -> Bool
isException = isLeft . runExcept

deriving instance MonadError e (Either e) => MonadError e (Except e)

instance IsString e => MonadFail (Except e) where fail = Except . Left . fromString  

instance One (Fwd (Except e) u u) (Fwd ((->) (Fwd (Except e) u u)) u) where one = Fwd id
instance One u (Bwd (Except e) u) where one = Bwd pure

instance StripPrefix u (Fwd (Except e) u) where stripPrefix = const $ pure ()
instance StripPrefix u (Bwd (Except e) u) where stripPrefix = const $ pure ()

instance Peek (Except e) where peek = id
instance Try (Except e) where try = id
instance OnError (Except e) where onError = const
