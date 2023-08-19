module Biparse.Constructor
  ( Constructor(..)
  , Focus
  , focus
  , FocusOne
  , focusOne
  , focusOneDef
  , lensBiparse
  , expectFwd
  , expose
  , exposes
  , Fwd(..)
  , Bwd(..)
  , (:*:)(..)
  , Comap(..)
  ) where

import Control.Monad.ChangeMonad (ChangeMonad(ChangeFunction,changeMonad))
--import Biparse.Biparser (Biparser(Biparser), Iso, forward, backward, one, SubElement, SubState, ElementContext)
import Biparse.Biparser (Biparser(Biparser), SubState, SubElement, one, Iso, GetSubState, UpdateStateWithElement)
import Biparse.Context.IdentityState (IdentityState)
import Biparse.Biparser.StateWriter qualified as BSW
import Control.Lens (Traversal', preview, assign)
import GHC.Generics (Generic, Generic1)
import Control.Monad.TransformerBaseMonad (TransformerBaseMonad, LiftBaseMonad, liftBaseMonad)
import Control.Monad.Reader (ReaderT(ReaderT), runReaderT, ask)
import Data.Default (Default, def)
import Control.Monad.Trans (lift)
import Control.Monad.StateError (runStateErrorT)
import Generic.Data (gpure, gap, gempty, galt)

newtype Constructor s m n u v = Constructor {deconstruct :: (Fwd (ReaderT s m) :*: Bwd (StateT s n)) u v}
  deriving (Functor, Applicative, Alternative, Monad, MonadFail)

type instance BwdMonad (Constructor _ _ n) = n
deriving via (Fwd (ReaderT s m) :*: Bwd (StateT s n)) instance Monad n => Comap (Constructor s m n)

type Focus m m' n n' =
  -- m
  ( Monad m
  , ChangeMonad () m' m
  , () ~ ChangeFunction () m' m
  -- n
  , Monad n
  , LiftBaseMonad n
  -- n'
  , n' ~ TransformerBaseMonad n
  , Monad n'
  )
focus :: forall c s m m' n n' u v u' s'.
  Focus m m' n n'
  => (u -> n' u')
  -> (u' -> s')
  -> Iso c m n s s'
  -> Constructor s' m' n' u' v
  -> Biparser c s m n u v
focus f g (Biparser fw bw) (Constructor (Fwd fw' :*: Bwd bw')) = Biparser
  do
    r <- fw
    changeMonad @() @m' @m () $ runReaderT fw' r
  \u -> do
    (v,s') <- liftBaseMonad do
      x <- f u
      runStateT (bw' x) $ g x
    _ <- bw s'
    pure v

type FocusOne c s m m' n n' ss se =
  ( IsSequence ss
  , GetSubState c s
  , UpdateStateWithElement c s
  -- m
  , MonadFail m
  , MonadState s m
  -- n
  , MonadWriter ss n
  -- assignments
  , ss ~ SubState c s
  , se ~ SubElement c s
  --
  , Focus m m' n n'
  )
focusOne :: forall c s m m' n n' u v ss se.
  FocusOne c s m m' n n' ss se
  => (u -> se)
  -> Constructor se m' n' u v
  -> Biparser c s m n u v
  -- => Constructor ss m' n' u v -> Biparser c s m n u v
focusOne f c = focus pure f one c

focusOneDef :: forall c s m m' n n' u v ss se.
  ( Default se
  , ChangeFunction () m' m ~ ()
  --
  , FocusOne c s m m' n n' ss se
  )
  => Constructor se m' n' u v
  -> Biparser c s m n u v
--focusOneDef c = focus pure (const def) one c
focusOneDef c = focusOne (const def) c

lensBiparse :: forall s s' m n u v.
  ( MonadFail m
  , Alternative m
  , Monad n
  )
  => Traversal' s s'
  -> BSW.Biparser IdentityState s' m n u v
  -> Constructor s m n u v
lensBiparse t (Biparser fw bw) = Constructor
  $   Fwd do
        s <- preview t >>= maybe (fail $ "lensBiparse could not preview") pure
        (v, _) <- ReaderT $ const $ runStateT (runStateErrorT fw) s
        pure v
  :*: Bwd \u -> do
        (v,w) <- lift $ runWriterT $ bw u
        assign t w
        pure v

expectFwd ::
  ( MonadFail m
  , Alternative m
  , Eq v
  , Show v
  , Monad n
  )
  => Traversal' s v
  -> v
  -> Constructor s m n u ()
expectFwd t x = Constructor
  $   Fwd do
        y <- preview t >>= maybe (fail $ "expectFwd could not preview when looking for: " <> show x) pure
        unless (x == y) $ fail $ "Expected '" <> show y <> "' to equal '" <> show x <> "'."
  :*: Bwd (const $ assign t x)

expose :: forall s m n u.
  ( Monad m
  , Monad n
  ) => Constructor s m n u s
expose = Constructor $ Fwd ask :*: Bwd (const get)

exposes :: forall s m n u a.
  ( Monad m
  , Monad n
  )
  => (s -> a)
  -> Constructor s m n u a
exposes = (<$> expose)

newtype Fwd m u v = Fwd {unFwd :: m v} deriving (Functor, Applicative, Alternative, Monad, MonadFail)

newtype Bwd m u v = Bwd {unBwd :: u -> m v} deriving (Functor, Generic1)
instance Applicative m => Applicative (Bwd m u) where
  pure = gpure
  (<*>) = gap
instance Monad m => Monad (Bwd m u) where
  Bwd bw >>= f = Bwd \u -> bw u >>= ($ u) . unBwd . f
instance Alternative m => Alternative (Bwd m u) where
  empty = Bwd $ const empty
  Bwd x <|> Bwd y = Bwd \u -> x u <|> y u
instance MonadFail m => MonadFail (Bwd m u) where
  fail = Bwd . const . fail

data (:*:) p q u v = (:*:) {pfst :: p u v, psnd :: q u v} deriving (Functor, Generic, Generic1)
instance (Applicative (p u), Applicative (q u)) => Applicative ((:*:) p q u) where
  pure = gpure
  (<*>) = gap
instance (Alternative (p u), Alternative (q u)) => Alternative ((:*:) p q u) where
  empty = gempty
  (<|>) = galt
instance (Monad (p u), Monad (q u)) => Monad ((:*:) p q u) where
  (fw :*: bw) >>= f = (fw >>= pfst . f) :*: (bw >>= psnd . f)
instance (MonadFail (p u), MonadFail (q u)) => MonadFail ((:*:) p q u) where
  fail msg = fail msg :*: fail msg

type BwdMonad :: (Type -> Type -> Type) -> Type -> Type
type family BwdMonad m
class Comap p where
  comap :: (u -> u') -> p u' v -> p u v
  comapM :: (u -> BwdMonad p u') -> p u' v -> p u v
  upon :: p u' v -> (u -> u') -> p u v
  upon = flip comap
  uponM :: p u' v -> (u -> BwdMonad p u') -> p u v
  uponM = flip comapM

type instance BwdMonad (_ :*: Bwd (StateT _ n)) = n
instance Monad n => Comap (Fwd m :*: Bwd (StateT s n)) where
  comap f (Fwd x :*: Bwd y) = Fwd x :*: Bwd (y . f)
  comapM f (Fwd x :*: Bwd y) = Fwd x :*: Bwd \u -> StateT \s -> f u >>= (flip runStateT s) . y

