{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Biparse.Constructor
  ( Constructor(Constructor, ..)
  --, Construct
  , runForwardC
  , runBackwardC
  , comap
  , comapM
  , upon
  , uponM
  , focusOneDef
  , FocusOne
  , focusOne
  , Focus
  , focus
  , lensBiparse
  , expect
  , expose
  , exposes
  ) where

import Control.Monad.ChangeMonad (ChangeMonad, ChangeFunction, changeMonad')
import Biparse.Biparser (Biparser, pattern Biparser, SubState, SubElement, one, Iso, GetSubState, UpdateStateWithElement)
import Biparse.Biparser.StateReaderWriter qualified as BSRW
import Lens.Micro (Traversal')
import Lens.Micro.Mtl (preview, assign)
import Control.Monad.TransformerBaseMonad (TransformerBaseMonad, LiftBaseMonad, liftBaseMonad)
import Control.Monad.Reader (ReaderT(ReaderT))
import Control.Monad.StateError (runStateErrorT)
import Control.Profunctor.FwdBwd (BwdMonad, Comap, FwdBwd, pattern FwdBwd, Fwd, Bwd)
import Control.Profunctor.FwdBwd qualified as FB

import Control.Monad.State qualified as S

newtype Constructor s m n u v = Constructor' {deconstruct :: FwdBwd (ReaderT s m) (S.StateT s n) u v}
  deriving (Functor, Applicative, Monad, MonadFail)
instance (Alt m, Alt n) => Alt (Constructor s m n u) where Constructor' x <!> Constructor' y = Constructor' $ x <!> y
pattern Constructor :: ReaderT s m v -> (u -> S.StateT s n v) -> Constructor s m n u v
pattern Constructor fw bw = Constructor' (FwdBwd fw bw)
{-# COMPLETE Constructor #-}
pattern ConstructorUnT :: (s -> m v) -> (u -> s -> n (v,s)) -> Constructor s m n u v
pattern ConstructorUnT fw bw <- Constructor (ReaderT fw) ((S.runStateT .) -> bw) where
  ConstructorUnT fw bw = Constructor (ReaderT fw) (S.StateT . bw)
{-# COMPLETE ConstructorUnT #-}

--type Construct m n s = Constructor s m n s s

runForwardC :: Constructor s m n u v -> s -> m v
runForwardC (ConstructorUnT fw _) = fw

runBackwardC :: Constructor s m n u v -> u -> s -> n (v,s)
runBackwardC (ConstructorUnT _ bw) = bw

data StateInstance
type instance BwdMonad StateInstance (_ FB.:*: Bwd (S.StateT _ n)) = n
instance Monad n => Comap StateInstance (Fwd m FB.:*: Bwd (S.StateT s n)) where
  comap f (FwdBwd x y) = FwdBwd x (y . f)
  comapM f (FwdBwd x y) = FwdBwd x \u -> S.StateT \s -> f u >>= flip S.runStateT s . y
type instance BwdMonad StateInstance (Constructor _ _ n) = n
-- | DEV NOTE: Should use Selectable
deriving via (Fwd (ReaderT s m) FB.:*: Bwd (S.StateT s n)) instance Monad n => Comap StateInstance (Constructor s m n)

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

-- * Focus on the head element

type FocusOne is c s m m' n n' ss se w =
  ( IsSequence ss
  , GetSubState s
  , UpdateStateWithElement c s
  -- m
  , MonadState s m
  , MonadFail m
  , Alt m
  -- n
  , MonadWriter w n
  -- w
  , ConvertElement c se w n
  -- assignments
  , ss ~ SubState s
  , se ~ SubElement s
  --
  , Focus is m m' n n'
  )

-- | 'focus' with 'one' and a 'Default' value.
-- When forward, runs the 'Constructor' one the first sub-element.
-- When backward, run the 'Constructor' on the 'Default' value of 'se'.
focusOneDef :: forall is m' n' se c s m n u v ss w.
  ( Default se
  , FocusOne is c s m m' n n' ss se w
  )
  => Constructor se m' n' u v
  -> Biparser c s m n u v
focusOneDef = focus @is pure (const def) one

focusOne :: forall is m' n' se c s m n u v ss w.
  FocusOne is c s m m' n n' ss se w
  => (u -> n' se)
  -> Constructor se m' n' se v
  -> Biparser c s m n u v
focusOne f c = focus @is f id one c

type Focus is m m' n n' =
  -- m
  ( Monad m
  , ChangeMonad is m' m
  , () ~ ChangeFunction is m' m
  -- n
  , Monad n
  , LiftBaseMonad n
  -- n'
  , n' ~ TransformerBaseMonad n
  , Monad n'
  )
-- | Uses 
focus :: forall is m' n' s' c s m n u v u'.
  Focus is m m' n n'
  => (u -> n' u') -- ^ Converts the contextual 'Biparser' 'u' to 'u\'' used by the 'Constructor' profunctor.
  -> (u' -> s') -- ^ Extracts the 's\'' to be used as the read-only variable in 'Fwd (ReaderT s\' m)' and the state in 'Bwd (StateT s\' n)'.
  -> Iso c m n s s' -- ^ The isomorphic biparser that translates between 's' and 's\''. Same use as in 'zoom'
  -> Constructor s' m' n' u' v -- ^ The 'Constructor' that when forward uses 's\'' to create 'v' and when backward modifies 's\'' for writing.
  -> Biparser c s m n u v
focus f g (Biparser fw bw) (ConstructorUnT fw' bw') = Biparser
  do
    r <- fw
    changeMonad' @is () $ fw' r
  \u -> do
    (v,s') <- liftBaseMonad do
      x <- f u
      bw' x $ g x
    _ <- bw s'
    pure v

-- * Constructor helper functions

lensBiparse :: forall c w s s' m n u v.
  ( MonadFail m
  , Monad n
  , BSRW.BackwardC c n w
  , ConvertSequence c w s' (S.StateT s n)
  )
  => Traversal' s s'
  -> BSRW.Biparser c (Identity s') m n () w () u v
  -> Constructor s m n u v
lensBiparse t (Biparser fw bw) = Constructor
  do
    s <- preview t >>= maybe (fail "lensBiparse could not preview") (pure . Identity)
    (v, _) <- ReaderT $ const $ runStateErrorT fw s
    pure v
  \u -> do
    (v,w) <- lift $ BSRW.runWriterT' @c $ bw u
    assign t =<< convertSequence @c w
    pure v

-- | Expect 'x' at 't' forward and set 'x' at 't' backwards.
expect :: forall s m n u v.
  ( MonadFail m
  , Eq v
  , Show v
  , Monad n
  )
  => Traversal' s v
  -> v
  -> Constructor s m n u ()
expect t x = Constructor
  do
    y <- preview t >>= maybe (fail $ "expect could not preview when looking for: " <> show x) pure
    unless (x == y) $ fail $ "Expected '" <> show y <> "' to equal '" <> show x <> "'."
  (const $ assign t x)

expose :: forall s m n u.
  ( Monad m
  , Monad n
  ) => Constructor s m n u s
expose = Constructor ask $ const get

exposes :: forall s m n u a.
  ( Monad m
  , Monad n
  )
  => (s -> a)
  -> Constructor s m n u a
exposes = (<$> expose)

