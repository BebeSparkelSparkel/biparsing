module Biparse.Constructor
  ( Constructor(..)
  , comap
  , comapM
  , upon
  , uponM
  , Focus
  , focus
  , FocusOne
  , focusOne
  , focusOneDef
  , lensBiparse
  , expectFwd
  , expose
  , exposes
  ) where

import Control.Monad.ChangeMonad (ChangeMonad(ChangeFunction), changeMonad)
import Biparse.Biparser (Biparser(Biparser), SubState, SubElement, one, Iso, GetSubState, UpdateStateWithElement)
import Biparse.Context.IdentityState (IdentityState)
import Biparse.Biparser.StateWriter qualified as BSW
import Control.Lens (Traversal', preview, assign)
import Control.Monad.TransformerBaseMonad (TransformerBaseMonad, LiftBaseMonad, liftBaseMonad)
import Control.Monad.Reader (ReaderT(ReaderT), runReaderT, ask)
import Data.Default (Default, def)
import Control.Monad.Trans (lift)
import Control.Monad.StateError (runStateErrorT)
import Control.Profunctor.FwdBwd ((:*:)((:*:)), Fwd(Fwd), Bwd(Bwd), BwdMonad, Comap)
import Control.Profunctor.FwdBwd qualified as FB

newtype Constructor s m n u v = Constructor {deconstruct :: (Fwd (ReaderT s m) :*: Bwd (StateT s n)) u v}
  deriving (Functor, Applicative, Alternative, Monad, MonadFail)

data StateInstance
type instance BwdMonad StateInstance (_ :*: Bwd (StateT _ n)) = n
instance Monad n => Comap StateInstance (Fwd m :*: Bwd (StateT s n)) where
  comap f (Fwd x :*: Bwd y) = Fwd x :*: Bwd (y . f)
  comapM f (Fwd x :*: Bwd y) = Fwd x :*: Bwd \u -> StateT \s -> f u >>= (flip runStateT s) . y
type instance BwdMonad StateInstance (Constructor _ _ n) = n
deriving via (Fwd (ReaderT s m) :*: Bwd (StateT s n)) instance Monad n => Comap StateInstance (Constructor s m n)

comap :: forall s m n u u' v.
  Monad n
  => (u -> u')
  -> Constructor s m n u' v
  -> Constructor s m n u v
comap = FB.comap @StateInstance

comapM :: forall s m n u u' v.
  Monad n
  => (u -> n u')
  -> Constructor s m n u' v
  -> Constructor s m n u v
comapM = FB.comapM @StateInstance

infix 8 `upon`
upon :: forall s m n u u' v.
  Monad n
  => Constructor s m n u' v
  -> (u -> u')
  -> Constructor s m n u v
upon = flip comap

infix 8 `uponM`
uponM :: forall s m n u u' v.
  Monad n
  => Constructor s m n u' v
  -> (u -> n u')
  -> Constructor s m n u v
uponM = flip comapM


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
focus :: forall m' n' s' c s m n u v u'.
  Focus m m' n n'
  => (u -> n' u')
  -> (u' -> s')
  -> Iso c m n s s'
  -> Constructor s' m' n' u' v
  -> Biparser c s m n u v
focus f g (Biparser fw bw) (Constructor (Fwd fw' :*: Bwd bw')) = Biparser
  do
    r <- fw
    changeMonad $ runReaderT fw' r
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
focusOne :: forall m' n' se c s m n u v ss.
  FocusOne c s m m' n n' ss se
  => (u -> se)
  -> Constructor se m' n' u v
  -> Biparser c s m n u v
  -- => Constructor ss m' n' u v -> Biparser c s m n u v
focusOne f c = focus pure f one c

focusOneDef :: forall m' n' se c s m n u v ss.
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

