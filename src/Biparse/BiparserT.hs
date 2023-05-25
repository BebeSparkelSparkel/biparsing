{-# LANGUAGE AllowAmbiguousTypes #-}
module Biparse.BiparserT
  ( BiparserT(forward, backward)
  , runForward
  , evalForward
  , runBackward
  , execBackward
  , Iso
  , iso
  , Unit
  , unit
  , identity
  , mapSW
  , mapSW'
  , comap
  , comapM
  , upon
  , uponM
  , emptyForward
  , StateContext(..)
  , one
  , try
  ) where

import Data.Functor (Functor(fmap), ($>), (<$>))
import Data.Tuple (fst, snd)
import Data.Eq (Eq((==)))
import Control.Monad (Monad((>>=)), MonadFail(fail), MonadPlus, (>=>), return)
import Data.Monoid (Monoid(mempty), (<>))
import Control.Applicative (Applicative(pure,(<*>)), Alternative(empty, (<|>)), (*>))
import Data.Function (flip, (.), const, ($), id, (&))
import Control.Monad.Loops (iterateUntilM)
import Control.Monad.Trans.State.Lazy (StateT(StateT, runStateT), get, put, gets)
import Control.Monad.Trans.Writer.Lazy (WriterT(WriterT, runWriterT), execWriterT, tell)
import Data.Profunctor (Profunctor(dimap), (:->))

import Data.Maybe (Maybe(Just,Nothing))
import Data.MonoTraversable (Element, headMay)
import Data.Sequences (IsSequence, tailMay, singleton)

data BiparserT context sw m n u v = BiparserT
  { forward :: StateT sw m v
  , backward :: u -> WriterT sw n v
  }

runForward :: forall c sw m n u v. BiparserT c sw m n u v -> sw -> m (v, sw)
runForward = runStateT . forward

evalForward :: forall c sw m n u v. Functor m => BiparserT c sw m n u v -> sw -> m v
evalForward = (fmap fst .) . runForward

runBackward :: forall c sw m n u v. BiparserT c sw m n u v -> u -> n (v, sw)
runBackward = (runWriterT .) . backward

execBackward :: forall c sw m n u v. Functor n => BiparserT c sw m n u v -> u -> n sw
execBackward = (fmap snd .) . runBackward

-- | Easy way to create new simple Biparser with Profunctor dimap
-- 'dimap (modify
identity :: (Monad m, Monad n) => Iso c m n a a
identity = BiparserT get (\u -> tell u $> u)

type Iso c m n a b = BiparserT c a m n b b
iso :: forall c m n a b. (Monad m, Monad n) => (a -> b) -> (b -> a) -> Iso c m n a b
iso f g = dimap g f identity

type Unit c sw m n = BiparserT c sw m n () ()
unit :: forall c sw m n u. Unit c sw m n -> BiparserT c sw m n u ()
unit = comap $ const ()

mapSW
  :: forall c sw sw' m.
   ( Monoid sw
   , Monoid sw'
   , Eq sw'
   , Monad m
   )
  =>  Iso c m m sw sw'
  ->  BiparserT c sw' m m
  :-> BiparserT c sw  m m
BiparserT fw' bw' `mapSW` BiparserT fw'' bw'' = BiparserT
  (   StateT
  $   runStateT fw'
  >=> \(ns,s) -> runStateT fw'' ns
  >>= \(x,ns') -> execWriterT (iterateUntilM (== mempty) bw' ns')
  >>= \s' -> pure (x, s' <> s)
  )
  (\u -> WriterT $ runWriterT (bw'' u) >>= \(x,w) -> runWriterT $ iterateUntilM (== mempty) bw' w $> x)

-- | Discards unused sw' state to avoid commingling m and n monads.
mapSW'
  :: forall c sw sw' m n.
   ( MonadFail m
   , Monad n
   )
  =>  Iso c m n sw sw'
  ->  BiparserT c sw' m n
  :-> BiparserT c sw  m n
BiparserT fw' bw' `mapSW'` BiparserT fw'' bw'' = BiparserT
  (StateT $ runStateT fw' >=> \(s,s') -> runStateT fw'' s >>= \(x, _) -> pure (x, s'))
  (\u -> WriterT $ runWriterT (bw'' u) >>= \(x,w) -> runWriterT (bw' w) >>= \(_,w') -> pure (x,w'))

comap :: forall c sw m n u u' v. (u -> u') -> BiparserT c sw m n u' v -> BiparserT c sw m n u v
comap f (BiparserT fw bw) = BiparserT fw (bw . f)

comapM :: forall c sw m n u u' v. Monad n => (u -> n u') -> BiparserT c sw m n u' v -> BiparserT c sw m n u v
comapM f (BiparserT fw bw) = BiparserT fw (\u -> WriterT $ f u >>= runWriterT . bw)

infix 8 `upon`
upon :: forall c sw m n u u' v. BiparserT c sw m n u' v -> (u -> u') -> BiparserT c sw m n u v
upon = flip comap

infix 8 `uponM`
uponM :: forall c sw m n u u' v. Monad n => BiparserT c sw m n u' v -> (u -> n u') -> BiparserT c sw m n u v
uponM = flip comapM

-- | Use instead of 'empty' when only forward should fail but backward should continue
emptyForward :: forall c sw m n u. (Monoid sw, MonadPlus m, Applicative n) => BiparserT c sw m n u ()
emptyForward = BiparserT empty (const $ pure ())

class StateContext context state subState where
  setStateContext :: state -> Element subState -> subState -> state
  getSubState :: state -> subState

data IdentityStateContext
instance StateContext IdentityStateContext state state where
  setStateContext _ _ s = s
  getSubState = id

instance (Functor m, Functor n) => Functor (BiparserT c sw m n u) where
  fmap f (BiparserT fw' bw') = BiparserT
    (fmap f fw')
    (fmap f . bw')

instance (Monoid sw, Monad m, Applicative n) => Applicative (BiparserT c sw m n u) where
  pure v = BiparserT (pure v) (const $ pure v)
  BiparserT fw' bw' <*> BiparserT fw'' bw'' = BiparserT
    (fw' <*> fw'')
    (\u -> bw' u <*> bw'' u)

instance (Monoid sw, MonadPlus m, Alternative n) => Alternative (BiparserT c sw m n u) where
  empty = BiparserT empty (const empty)
  BiparserT fw' bw' <|> BiparserT fw'' bw'' =
    BiparserT (fw' <|> fw'') (\u -> bw' u <|> bw'' u)

instance (Monoid sw, Monad m, Monad n) => Monad (BiparserT c sw m n u) where
  pu >>= kw = BiparserT fw' bw'
    where
    fw' = forward pu >>= forward . kw
    bw' u = backward pu u >>= ($ u) . backward . kw

instance (Monoid sw, MonadFail m, MonadFail n) => MonadFail (BiparserT c sw m n u) where
  fail x = BiparserT (fail x) (const $ fail x)

instance (Functor m, Functor n) => Profunctor (BiparserT c sw m n) where
  dimap f g (BiparserT fw' bw') = BiparserT (g <$> fw') (fmap g . bw' . f)


-- | Takes and Writes one element.
one :: forall c sw m n subState.
  ( IsSequence subState
  , MonadFail m
  , Monad n
  , StateContext c sw subState
  ) => BiparserT c sw m n (Element subState) (Element subState)
one = BiparserT
  ( do
    s <- get
    headTailMay (getSubState s) & \case
      Just (x, ss) -> put (setStateContext s x ss) $> x
      Nothing -> fail "Could not take one element. The container is empty."
  )   
  --( gets getSubState >>= headTailMay >>> \case
  --    Just (c, s) -> put (setStateContext s c)  $> c
  --    Nothing -> fail "Could not take one element. The container is empty."
  --)
  (\c -> tell (singleton c) $> c)
  where
  headTailMay :: IsSequence b => b -> Maybe (Element b, b)
  headTailMay x = do
    h <- headMay x
    sw <- tailMay x
    return (h, sw)

-- | Allows trying a forward. If the forward fails the state is returned to the value it was before running.
-- ????? Unsure what should be done with the writer mondad
try :: MonadPlus m => BiparserT c sw m n u v -> BiparserT c sw m n u v
try (BiparserT fw bw) = BiparserT
  do
    s <- get
    fw <|> put s *> empty
  bw

(>>>) :: (a -> b) -> (b -> c) -> a -> c
(>>>) = flip (.)

