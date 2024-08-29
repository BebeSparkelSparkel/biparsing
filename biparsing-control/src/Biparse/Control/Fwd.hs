module Biparse.Control.Fwd (
Fwd(..),
StateSeq,
pattern StateSeq,
) where

import Data.Bifunctor.Joker (Joker(Joker))
import Profunctor.Monad.Cofunctor (First, lmap)
import Data.Sequences (IsSequence(uncons))
import Data.Sequences qualified as S
import Data.Bifunctor (Bifunctor)
import Biparse.Control.BP (BP(BP))

-- | 'u' is thrown away
newtype Fwd m u a = Fwd {runFwd :: m a}
  deriving (Show, Eq, Functor, Applicative, Alternative, Monad, MonadFail, Peek, Try)
  deriving (MonadState s, MonadError e) via m
  deriving Profunctor via Joker m

instance Monad m => Cofunctor (Fwd m) where
  type First (Fwd m) = Kleisli m
  lmap = const coerce

type instance Item' (Fwd m) = Item' m
--instance OneFwd a m => One a (Fwd m) where one = Fwd oneFwd

type instance WhichDirection (Fwd _ _) = 'Forward

deriving via Fwd m u instance One u (Fwd m u) => One u (Fwd (IdentityT m) u)
instance One u (Fwd m u) => One u (Fwd (Kleisli m b) u) where one = Fwd $ Kleisli $ const $ runFwd @_ @u one
deriving via Fwd (Kleisli m r) u instance One u (Fwd m u) => One u (Fwd (ReaderT r m) u)
instance (One u (Fwd m u), Monad m) => One u (Fwd (CPSWriterT w m) u) where one = Fwd $ lift $ runFwd @_ @u one
instance (One u (Fwd m u), Monad m, Monoid w) => One u (Fwd (LazyWriterT w m) u) where one = Fwd $ lift $ runFwd @_ @u one
instance (One u (Fwd m u), Monad m, Monoid w) => One u (Fwd (StrictWriterT w m) u) where one = Fwd $ lift $ runFwd @_ @u one
instance (One u (Fwd m u), Monad m) => One u (Fwd (CPSRWST r w s m) u) where one = Fwd $ lift $ runFwd @_ @u one
instance (One u (Fwd m u), Monad m, Monoid w) => One u (Fwd (LazyRWST r w s m) u) where one = Fwd $ lift $ runFwd @_ @u one
instance (One u (Fwd m u), Monad m, Monoid w) => One u (Fwd (StrictRWST r w s m) u) where one = Fwd $ lift $ runFwd @_ @u one
instance (u ~ Element seq, One (Fwd m u u) (Fwd ((->) (Fwd m u u)) u), UpdateStateWithElement s u, IsSequence seq, MonadFail m) => One u (Fwd (BP (LazyStateT (StateSeq s seq) m)) u) where one = oneState
instance (u ~ Element seq, One (Fwd m u u) (Fwd ((->) (Fwd m u u)) u), UpdateStateWithElement s u, IsSequence seq, MonadFail m) => One u (Fwd (BP (StrictStateT (StateSeq s seq) m)) u) where one = oneState
instance (u ~ Element seq, One (Fwd m u u) (Fwd ((->) (Fwd m u u)) u), UpdateStateWithElement s u, IsSequence seq, Monoid w, MonadFail m) => One u (Fwd (BP (CPSRWST r w (StateSeq s seq) m)) u) where one = oneState
instance (u ~ Element seq, One (Fwd m u u) (Fwd ((->) (Fwd m u u)) u), UpdateStateWithElement s u, IsSequence seq, Monoid w, MonadFail m) => One u (Fwd (BP (LazyRWST r w (StateSeq s seq) m)) u) where one = oneState
instance (u ~ Element seq, One (Fwd m u u) (Fwd ((->) (Fwd m u u)) u), UpdateStateWithElement s u, IsSequence seq, Monoid w, MonadFail m) => One u (Fwd (BP (StrictRWST r w (StateSeq s seq) m)) u) where one = oneState
oneState :: forall t m s seq u.
  ( MonadState (StateSeq s seq) (t m)
  , u ~ Element seq
  , UpdateStateWithElement s u
  , IsSequence seq
  , MonadFail (t m)
  , Monad m
  , MonadTrans t
  , One (Fwd m u u) (Fwd ((->) (Fwd m u u)) u)
  ) => Fwd (BP (t m)) u u
oneState = Fwd $ BP do
  StateSeq s xs <- get
  (x, xs') <- maybe (fail "Unexpected end of input.") pure $ uncons xs
  put $ StateSeq (updateStateWithElement x s) xs'
  lift $ runFwd @_ @u $ runFwd @_ @u one $ pure @(Fwd m u) x
instance One (Fwd IO u u) (Fwd ((->) (Fwd IO u u)) u) where one = Fwd id
instance One (Fwd Maybe u u) (Fwd ((->) (Fwd Maybe u u)) u) where one = Fwd id
instance One (Fwd m u u) (Fwd ((->) (Fwd m u u)) u) => One (Fwd (IdentityT m) u u) (Fwd ((->) (Fwd (IdentityT m) u u)) u) where one = coerce (one :: Fwd ((->) (Fwd m u u)) u (Fwd m u u))
instance One (Fwd m u u) (Fwd ((->) (Fwd m u u)) u) => One (Fwd (Kleisli m a) u u) (Fwd ((->) (Fwd (Kleisli m a) u u)) u) where one = Fwd \(Fwd (Kleisli f)) -> Fwd $ Kleisli \x -> coerce (one :: (Fwd ((->) (Fwd m u u)) u) (Fwd m u u)) $ f x
instance One (Fwd m u u) (Fwd ((->) (Fwd m u u)) u) => One (Fwd (ReaderT r m) u u) (Fwd ((->) (Fwd (ReaderT r m) u u)) u) where one = Fwd \(Fwd (ReaderT f)) -> Fwd $ ReaderT \x -> coerce (one :: (Fwd ((->) (Fwd m u u)) u) (Fwd m u u)) $ f x
instance (One (Fwd m (u, w) (u, w)) (Fwd ((->) (Fwd m (u, w) (u, w))) (u, w)), Functor m, Monoid w) => One (Fwd (CPSWriterT w m) u u) (Fwd ((->) (Fwd (CPSWriterT w m) u u)) u) where one = Fwd \(Fwd (CPSWriterT x)) -> Fwd $ CPSWriterT $ coerce (one :: (Fwd ((->) (Fwd m (u, w) (u, w))) (u, w)) (Fwd m (u, w) (u, w))) $ x
instance One (Fwd m (u, w) (u, w)) (Fwd ((->) (Fwd m (u, w) (u, w))) (u, w)) => One (Fwd (LazyWriterT w m) u u) (Fwd ((->) (Fwd (LazyWriterT w m) u u)) u) where one = Fwd \(Fwd (LazyWriterT x)) -> Fwd $ LazyWriterT $ coerce (one :: (Fwd ((->) (Fwd m (u, w) (u, w))) (u, w)) (Fwd m (u, w) (u, w))) $ x
instance One (Fwd m (u, w) (u, w)) (Fwd ((->) (Fwd m (u, w) (u, w))) (u, w)) => One (Fwd (StrictWriterT w m) u u) (Fwd ((->) (Fwd (StrictWriterT w m) u u)) u) where one = Fwd \(Fwd (StrictWriterT x)) -> Fwd $ StrictWriterT $ coerce (one :: (Fwd ((->) (Fwd m (u, w) (u, w))) (u, w)) (Fwd m (u, w) (u, w))) $ x
instance One (Fwd m (u, s) (u, s)) (Fwd ((->) (Fwd m (u, s) (u, s))) (u, s)) => One (Fwd (LazyStateT s m) u u) (Fwd ((->) (Fwd (LazyStateT s m) u u)) u) where one = Fwd \(Fwd (LazyStateT f)) -> Fwd $ LazyStateT \s -> coerce (one :: (Fwd ((->) (Fwd m (u, s) (u, s))) (u, s)) (Fwd m (u, s) (u, s))) $ f s
instance One (Fwd m (u, s) (u, s)) (Fwd ((->) (Fwd m (u, s) (u, s))) (u, s)) => One (Fwd (StrictStateT s m) u u) (Fwd ((->) (Fwd (StrictStateT s m) u u)) u) where one = Fwd \(Fwd (StrictStateT f)) -> Fwd $ StrictStateT \s -> coerce (one :: (Fwd ((->) (Fwd m (u, s) (u, s))) (u, s)) (Fwd m (u, s) (u, s))) $ f s
instance (One (Fwd m (u, s, w) (u, s, w)) (Fwd ((->) (Fwd m (u, s, w) (u, s, w))) (u, s, w)), Functor m, Monoid w) => One (Fwd (CPSRWST r w s m) u u) (Fwd ((->) (Fwd (CPSRWST r w s m) u u)) u) where one = Fwd \(Fwd (CPSRWST f)) -> Fwd $ CPSRWST \r s -> coerce (one :: (Fwd ((->) (Fwd m (u, s, w) (u, s, w))) (u, s, w)) (Fwd m (u, s, w) (u, s, w))) $ f r s
instance One (Fwd m (u, s, w) (u, s, w)) (Fwd ((->) (Fwd m (u, s, w) (u, s, w))) (u, s, w)) => One (Fwd (LazyRWST r w s m) u u) (Fwd ((->) (Fwd (LazyRWST r w s m) u u)) u) where one = Fwd \(Fwd (LazyRWST f)) -> Fwd $ LazyRWST \r s -> coerce (one :: (Fwd ((->) (Fwd m (u, s, w) (u, s, w))) (u, s, w)) (Fwd m (u, s, w) (u, s, w))) $ f r s
instance One (Fwd m (u, s, w) (u, s, w)) (Fwd ((->) (Fwd m (u, s, w) (u, s, w))) (u, s, w)) => One (Fwd (StrictRWST r w s m) u u) (Fwd ((->) (Fwd (StrictRWST r w s m) u u)) u) where one = Fwd \(Fwd (StrictRWST f)) -> Fwd $ StrictRWST \r s -> coerce (one :: (Fwd ((->) (Fwd m (u, s, w) (u, s, w))) (u, s, w)) (Fwd m (u, s, w) (u, s, w))) $ f r s

--instance BiN seq (Fwd (BP (LazyStateT (StateSeq s seq) m))) where
--  biN = _
--class BiNState a s where biNState :: (MonadState s m, MonadFail m) => m a
--instance
--  (
--  ) => BiNState seq (StateSeq s seq) where
--  biNState = do
--    StateSeq s xs <- get
--    (x, xs') <- maybe (fail "Unexpected end of input.") pure $ uncons xs
--    put $ StateSeq (updateStateWithElement x s) xs'
--    return x

instance StripPrefix u (Fwd IO u) where stripPrefix = const $ pure ()
instance StripPrefix u (Fwd Maybe u) where stripPrefix = const $ pure ()
deriving via Fwd m u instance StripPrefix u (Fwd m u) => StripPrefix u (Fwd (IdentityT m) u)
instance StripPrefix u (Fwd m u) => StripPrefix u (Fwd (Kleisli m a) u) where stripPrefix prefix = Fwd $ Kleisli $ const $ runFwd @_ @u $ stripPrefix prefix
deriving via Fwd (Kleisli m r) u instance StripPrefix u (Fwd m u) => StripPrefix u (Fwd (ReaderT r m) u)

instance (Monad m, StripPrefix u (Fwd m u)) => StripPrefix u (Fwd (CPSWriterT w m) u) where stripPrefix = Fwd . lift . runFwd @_ @u . stripPrefix
instance (Monad m, StripPrefix u (Fwd m u), Monoid w) => StripPrefix u (Fwd (LazyWriterT w m) u) where stripPrefix = Fwd . lift . runFwd @_ @u . stripPrefix
instance (Monad m, StripPrefix u (Fwd m u), Monoid w) => StripPrefix u (Fwd (StrictWriterT w m) u) where stripPrefix = Fwd . lift . runFwd @_ @u . stripPrefix
instance (Monad m, StripPrefix u (Fwd m u)) => StripPrefix u (Fwd (LazyStateT s m) u) where stripPrefix = Fwd . lift . runFwd @_ @u . stripPrefix
instance (Monad m, StripPrefix u (Fwd m u)) => StripPrefix u (Fwd (StrictStateT s m) u) where stripPrefix = Fwd . lift . runFwd @_ @u . stripPrefix
instance (Monad m, StripPrefix u (Fwd m u)) => StripPrefix u (Fwd (CPSRWST r w s m) u) where stripPrefix = Fwd . lift . runFwd @_ @u . stripPrefix
instance (Monad m, StripPrefix u (Fwd m u), Monoid w) => StripPrefix u (Fwd (LazyRWST r w s m) u) where stripPrefix = Fwd . lift . runFwd @_ @u . stripPrefix
instance (Monad m, StripPrefix u (Fwd m u), Monoid w) => StripPrefix u (Fwd (StrictRWST r w s m) u) where stripPrefix = Fwd . lift . runFwd @_ @u . stripPrefix
instance (Show seq, Eq (Element seq), IsSequence seq, UpdateStateWithSequence s seq, StripPrefix seq (Fwd m seq), MonadFail m) => StripPrefix seq (Fwd (BP (LazyStateT (StateSeq s seq) m)) u) where stripPrefix = stripPrefixState
instance (Show seq, Eq (Element seq), IsSequence seq, UpdateStateWithSequence s seq, StripPrefix seq (Fwd m seq), MonadFail m) => StripPrefix seq (Fwd (BP (StrictStateT (StateSeq s seq) m)) u) where stripPrefix = stripPrefixState
instance (Show seq, Eq (Element seq), IsSequence seq, UpdateStateWithSequence s seq, StripPrefix seq (Fwd m seq), Monoid w, MonadFail m) => StripPrefix seq (Fwd (BP (CPSRWST r w (StateSeq s seq) m)) u) where stripPrefix = stripPrefixState
instance (Show seq, Eq (Element seq), IsSequence seq, UpdateStateWithSequence s seq, StripPrefix seq (Fwd m seq), Monoid w, MonadFail m) => StripPrefix seq (Fwd (BP (LazyRWST r w (StateSeq s seq) m)) u) where stripPrefix = stripPrefixState
instance (Show seq, Eq (Element seq), IsSequence seq, UpdateStateWithSequence s seq, StripPrefix seq (Fwd m seq), Monoid w, MonadFail m) => StripPrefix seq (Fwd (BP (StrictRWST r w (StateSeq s seq) m)) u) where stripPrefix = stripPrefixState
stripPrefixState :: forall seq s t m u.
  ( Show seq
  , Eq (Element seq)
  , IsSequence seq
  , UpdateStateWithSequence s seq
  , MonadState (StateSeq s seq) (t m)
  , MonadFail (t m)
  , MonadTrans t
  , Monad m
  , StripPrefix seq (Fwd m seq)
  ) => seq -> Fwd (BP (t m)) u ()
stripPrefixState prefix = Fwd $ BP do
  lift $ runFwd @_ @seq $ stripPrefix prefix
  StateSeq s seq <- get
  S.stripPrefix prefix seq & maybe
    (fail $ "Could not match prefix: " <> show prefix)
    \remainder -> do
      put $ StateSeq (updateStateWithSequence prefix s) remainder

newtype StateSeq s seq = StateSeq' (s, seq) deriving (Show, Eq, Bifunctor)
{-# COMPLETE StateSeq #-}
pattern StateSeq :: s -> seq -> StateSeq s seq
pattern StateSeq s seq = StateSeq' (s, seq)
instance (Default s, IsString seq) => IsString (StateSeq s seq) where fromString = StateSeq def . fromString
instance (Default s, Monoid seq) => Default (StateSeq s seq) where def = StateSeq def mempty
