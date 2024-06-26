{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}
module Control.Profunctor.FwdBwd
  ( (:*:)(..)
  , Fwd(..)
  , Bwd(..)
  , BwdMonad
  , Comap(..)
  , FwdBwd
  , pattern FwdBwd
  , MapMs(..)
  , DualMap(..)
  ) where

import GHC.Generics (Generic, Generic1)
import Control.Monad.Unrecoverable (MonadUnrecoverable, UnrecoverableError, throwUnrecoverable)
import Data.Profunctor (Profunctor(dimap))

data (:*:) p q u v = (:*:) {pfst :: p u v, psnd :: q u v} deriving (Show, Eq, Functor, Generic, Generic1)
{-# COMPLETE (:*:) #-}
instance (Applicative (p u), Applicative (q u)) => Applicative ((:*:) p q u) where
  pure x = pure x :*: pure x
  (f :*: b) <*> ~(f' :*: b') = (f <*> f') :*: (b <*> b')
instance (Alt (p u), Alt (q u)) => Alt ((:*:) p q u) where
  (f :*: b) <!> ~(f' :*: b') = (f <!> f') :*: (b <!> b')
instance (Monad (p u), Monad (q u)) => Monad ((:*:) p q u) where
  (fw :*: bw) >>= f = (fw >>= pfst . f) :*: (bw >>= psnd . f)
instance (MonadFail (p u), MonadFail (q u)) => MonadFail ((:*:) p q u) where
  fail msg = fail msg :*: fail msg
instance (MonadError e (p u), MonadError e (q u)) => MonadError e ((:*:) p q u) where
  throwError e = throwError e :*: throwError e
  catchError (fw :*: bw) eh = catchError fw (pfst . eh) :*: catchError bw (psnd . eh)
instance (MonadUnrecoverable (p u), MonadUnrecoverable (q u), UnrecoverableError (p u) ~ UnrecoverableError (q u)) => MonadUnrecoverable ((:*:) p q u) where
  type UnrecoverableError ((:*:) p q u) = UnrecoverableError (p u)
  throwUnrecoverable e = throwUnrecoverable e :*: throwUnrecoverable e
instance (Profunctor p, Profunctor q) => Profunctor ((:*:) p q) where
  dimap f g (l :*: r) = dimap f g l :*: dimap f g r

newtype Fwd m u v = Fwd {unFwd :: m v} deriving (Show, Eq, Functor, Applicative, Monad, MonadFail)
deriving instance MonadError e m => MonadError e (Fwd m u)
deriving instance MonadUnrecoverable m => MonadUnrecoverable (Fwd m u)
instance Alt m => Alt (Fwd m u) where
  Fwd x <!> Fwd y = Fwd $ x <!> y
instance Functor m => Profunctor (Fwd m) where
  dimap _ g (Fwd x) = Fwd $ g <$> x

newtype Bwd m u v = Bwd {unBwd :: u -> m v} deriving (Functor, Generic1)
instance (Default u, Show (m v)) => Show (Bwd m u v) where
  show (Bwd f) = "Bwd " <> show (f def)
instance (Default u, Eq (m v)) => Eq (Bwd m u v) where
  Bwd f == Bwd g = f def == g def
instance Applicative m => Applicative (Bwd m u) where
  pure = Bwd . const . pure
  Bwd x <*> Bwd y = Bwd \u -> x u <*> y u
instance Monad m => Monad (Bwd m u) where
  Bwd bw >>= f = Bwd \u -> bw u >>= ($ u) . unBwd . f
instance Alt m => Alt (Bwd m u) where
  Bwd x <!> Bwd y = Bwd \u -> x u <!> y u
instance MonadFail m => MonadFail (Bwd m u) where
  fail = Bwd . const . fail
instance MonadError e m => MonadError e (Bwd m u) where
  throwError = Bwd . const . throwError
  catchError (Bwd x) f = Bwd \u -> catchError (x u) $ ($ u) . unBwd . f
instance MonadUnrecoverable m => MonadUnrecoverable (Bwd m u) where
  type UnrecoverableError (Bwd m u) = UnrecoverableError m
  throwUnrecoverable = Bwd . const . throwUnrecoverable
instance Functor m => Profunctor (Bwd m) where
  dimap f g (Bwd x) = Bwd $ fmap g . x . f

type BwdMonad :: Type -> (Type -> Type -> Type) -> Type -> Type
type family BwdMonad instanceSelector m
--infix 8 `upon`, `uponM`
class Comap is (p :: Type -> Type -> Type) where
  comap :: (u -> u') -> p u' v -> p u v
  comapM :: (u -> BwdMonad is p u') -> p u' v -> p u v
  --upon :: p u' v -> (u -> u') -> p u v
  --upon = flip $ comap @is
  --uponM :: p u' v -> (u -> BwdMonad is p u') -> p u v
  --uponM = flip $ comapM @is

type instance BwdMonad () (_ :*: Bwd n) = n
instance Monad n => Comap () (Fwd m :*: Bwd n) where
  comap  f (Fwd fw :*: Bwd bw) = Fwd fw :*: Bwd (bw . f)
  comapM f (Fwd fw :*: Bwd bw) = Fwd fw :*: Bwd (bw <=< f)

type FwdBwd m n = Fwd m :*: Bwd n
pattern FwdBwd :: m v -> (u -> n v) -> (Fwd m :*: Bwd n) u v
pattern FwdBwd fw bw = Fwd fw :*: Bwd bw
{-# COMPLETE FwdBwd #-}

class MapMs p where
  mapMs
    :: (forall a. m a -> m' a)
    -> (forall a. n a -> n' a)
    -> p m  n  u v
    -> p m' n' u v
  firstM
    :: (forall a. m a -> m' a)
    -> p m  n u v
    -> p m' n u v
  firstM x = mapMs x id
  secondM
    :: (forall a. n a -> n' a)
    -> p m n  u v
    -> p m n' u v
  secondM = mapMs id

--instance MapMs (:*:) where
--  mapMs f g (x :*: y) = f x :*: g y

class DualMap f where
  dualMap :: (a -> b) -> (a -> b) -> f a -> f b
  endoFirst :: (a -> a) -> f a -> f a
  endoFirst f = dualMap f id
  endoSecond :: (a -> a) -> f a -> f a
  endoSecond = dualMap id

instance (Functor m, Functor n) => DualMap (FwdBwd m n u) where
  dualMap f g (m :*: n) = fmap f m :*: fmap g n

