module Biparse.Control.Bwd (Bwd(..)) where

import Profunctor.Monad.Cofunctor (First, lmap)
import Biparse.Control.BP (BP(BP))

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

instance Monad m => Cofunctor (Bwd m) where
  type First (Bwd m) = Kleisli m
  lmap (Kleisli f) (Bwd x) = Bwd $ x <=< f

instance Peek (Bwd m u) where peek = id
instance Try (Bwd m u) where try = id

type instance WhichDirection (Bwd _ _) = 'Backward

instance One u (Bwd IO u) where one = Bwd pure
instance One u (Bwd Maybe u) where one = Bwd pure
deriving via Bwd m u instance One u (Bwd m u) => One u (Bwd (IdentityT m) u)
instance One u (Bwd m u) => One u (Bwd (Kleisli m a) u) where one = Bwd \u -> Kleisli $ const $ runBwd one u
deriving via Bwd (Kleisli m r) u instance One u (Bwd m u) => One u (Bwd (ReaderT r m) u)
instance (One u (Bwd m u), Monad m) => One u (Bwd (CPSWriterT w m) u) where one = Bwd $ lift . runBwd one
instance (One u (Bwd m u), Monad m, Monoid w) => One u (Bwd (LazyWriterT w m) u) where one = Bwd $ lift . runBwd one
instance (One u (Bwd m u), Monad m, Monoid w) => One u (Bwd (StrictWriterT w m) u) where one = Bwd $ lift . runBwd one
instance (One u (Bwd m u), Monad m) => One u (Bwd (LazyStateT w m) u) where one = Bwd $ lift . runBwd one
instance (One u (Bwd m u), Monad m) => One u (Bwd (StrictStateT w m) u) where one = Bwd $ lift . runBwd one
instance (One u (Bwd m u), Monad m) => One u (Bwd (CPSRWST r w s m) u) where one = Bwd $ lift . runBwd one
instance (One u (Bwd m u), Monad m, Monoid w) => One u (Bwd (LazyRWST r w s m) u) where one = Bwd $ lift . runBwd one
instance (One u (Bwd m u), Monad m, Monoid w) => One u (Bwd (StrictRWST r w s m) u) where one = Bwd $ lift . runBwd one
instance (Element w ~ u, Monoid w, MonoPointed w, Monad m, One u (Bwd m u)) => One u (Bwd (BP (CPSWriterT w m)) u) where one = oneWrite
instance (Element w ~ u, Monoid w, MonoPointed w, Monad m, One u (Bwd m u)) => One u (Bwd (BP (LazyWriterT w m)) u) where one = oneWrite
instance (Element w ~ u, Monoid w, MonoPointed w, Monad m, One u (Bwd m u)) => One u (Bwd (BP (StrictWriterT w m)) u) where one = oneWrite
instance (Element w ~ u, Monoid w, MonoPointed w, Monad m, One u (Bwd m u)) => One u (Bwd (BP (CPSRWST r w s m)) u) where one = oneWrite
instance (Element w ~ u, Monoid w, MonoPointed w, Monad m, One u (Bwd m u)) => One u (Bwd (BP (LazyRWST r w s m)) u) where one = oneWrite
instance (Element w ~ u, Monoid w, MonoPointed w, Monad m, One u (Bwd m u)) => One u (Bwd (BP (StrictRWST r w s m)) u) where one = oneWrite
oneWrite ::
  ( MonadTrans t
  , Monad m
  , MonadWriter w (t m)
  , MonoPointed w
  , One u (Bwd m u)
  , u ~ Element w
  ) => Bwd (BP (t m)) u u
oneWrite = Bwd $ \u -> BP $ lift (runBwd one u) <* tell (opoint u)

instance StripPrefix u (Bwd IO u) where stripPrefix = const $ pure ()
instance StripPrefix u (Bwd Maybe u) where stripPrefix = const $ pure ()
deriving via Bwd m u instance StripPrefix u (Bwd m u) => StripPrefix u (Bwd (IdentityT m) u)
instance StripPrefix u (Bwd m u) => StripPrefix u (Bwd (Kleisli m a) u) where stripPrefix prefix = Bwd $ Kleisli . const . runBwd (stripPrefix prefix)
deriving via Bwd (Kleisli m r) u instance StripPrefix u (Bwd m u) => StripPrefix u (Bwd (ReaderT r m) u)
instance (StripPrefix u (Bwd m u), Monad m) => StripPrefix u (Bwd (CPSWriterT w m) u) where stripPrefix prefix = Bwd $ lift . runBwd (stripPrefix prefix)
instance (StripPrefix u (Bwd m u), Monad m, Monoid w) => StripPrefix u (Bwd (LazyWriterT w m) u) where stripPrefix prefix = Bwd $ lift . runBwd (stripPrefix prefix)
instance (StripPrefix u (Bwd m u), Monad m, Monoid w) => StripPrefix u (Bwd (StrictWriterT w m) u) where stripPrefix prefix = Bwd $ lift . runBwd (stripPrefix prefix)
instance (StripPrefix u (Bwd m u), Monad m) => StripPrefix u (Bwd (LazyStateT s m) u) where stripPrefix prefix = Bwd $ lift . runBwd (stripPrefix prefix)
instance (StripPrefix u (Bwd m u), Monad m) => StripPrefix u (Bwd (StrictStateT s m) u) where stripPrefix prefix = Bwd $ lift . runBwd (stripPrefix prefix)
instance (StripPrefix u (Bwd m u), Monad m) => StripPrefix u (Bwd (CPSRWST r w s m) u) where stripPrefix prefix = Bwd $ lift . runBwd (stripPrefix prefix)
instance (StripPrefix u (Bwd m u), Monad m, Monoid w) => StripPrefix u (Bwd (LazyRWST r w s m) u) where stripPrefix prefix = Bwd $ lift . runBwd (stripPrefix prefix)
instance (StripPrefix u (Bwd m u), Monad m, Monoid w) => StripPrefix u (Bwd (StrictRWST r w s m) u) where stripPrefix prefix = Bwd $ lift . runBwd (stripPrefix prefix)
instance (StripPrefix w (Bwd m w), Monad m, Monoid w) => StripPrefix w (Bwd (BP (CPSWriterT w m)) u) where stripPrefix = stripPrefixWrite
instance (StripPrefix w (Bwd m w), Monad m, Monoid w) => StripPrefix w (Bwd (BP (LazyWriterT w m)) u) where stripPrefix = stripPrefixWrite
instance (StripPrefix w (Bwd m w), Monad m, Monoid w) => StripPrefix w (Bwd (BP (StrictWriterT w m)) u) where stripPrefix = stripPrefixWrite
instance (StripPrefix w (Bwd m w), Monad m, Monoid w) => StripPrefix w (Bwd (BP (CPSRWST r w s m)) u) where stripPrefix = stripPrefixWrite
instance (StripPrefix w (Bwd m w), Monad m, Monoid w) => StripPrefix w (Bwd (BP (LazyRWST r w s m)) u) where stripPrefix = stripPrefixWrite
instance (StripPrefix w (Bwd m w), Monad m, Monoid w) => StripPrefix w (Bwd (BP (StrictRWST r w s m)) u) where stripPrefix = stripPrefixWrite
stripPrefixWrite ::
  ( MonadTrans t
  , MonadWriter w (t m)
  , Monad m
  , StripPrefix w (Bwd m w)
  ) => w -> Bwd (BP (t m)) u ()
stripPrefixWrite prefix = Bwd $ const $ BP do
  lift $ runBwd (stripPrefix prefix) prefix
  tell prefix

