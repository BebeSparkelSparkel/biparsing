{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}
module Biparse.BiparserT
  ( BiparserT(forward, backward)
  , runForward
  , evalForward
  , runBackward
  , execBackward
  , Iso
  --, iso
  , Unit
  , unit
  , mono
  , Const
  , ConstU
  , mkConst
  , identity
  , mapState
  --, mapState'
  , mapMs
  , mapMs'
  , fix
  , fixWith
  , FixFail(..)
  , mapFW
  , comap
  , comapM
  , comapMay
  , upon
  , uponM
  , uponMay
  , mapBack
  , emptyForward
  , SubState
  , GetSubState(..)
  , UpdateStateWithSubState(..)
  , SubStateContext
  , SubElement
  , UpdateStateWithElement(..)
  , ElementContext
  , IdentityStateContext
  , one
  , split
  , peek
  , try
  , optionalBack
  ) where

import Data.Coerce (coerce)
import Data.Bifunctor.Flip (Flip(Flip))
import Data.Traversable (Traversable)
import Control.Applicative (Applicative(pure,(<*>)), Alternative(empty, (<|>)), (*>), (<*))
import Control.Monad (Monad((>>=)), MonadFail(fail), MonadPlus, (>=>), return, sequence)
import Control.Monad.Loops (iterateUntilM)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State.Lazy (StateT(StateT, runStateT), get, put, mapStateT)
import Control.Monad.Trans.Writer.Lazy (WriterT(WriterT, runWriterT), execWriterT, tell, mapWriterT)
import Data.Bool (bool)
import Data.Either (Either, fromRight)
import Data.Eq (Eq((==)))
import Data.Function (flip, (.), const, ($), id, (&))
import Data.Functor (Functor(fmap), ($>), (<$>))
import Data.Functor.Identity (Identity(Identity, runIdentity))
import Data.Kind (Type)
import Data.Maybe (Maybe(Just,Nothing), fromMaybe, maybe)
import Data.MonoTraversable (Element, headMay)
import Data.Monoid (Monoid(mempty), (<>))
import Data.Profunctor (Profunctor(dimap), (:->))
import Data.Sequences (IsSequence, tailMay, singleton)
import Data.Tuple (fst, snd, swap)

-- | Product type for simultainously constructing forward and backward running programs.
data BiparserT context s m n u v = BiparserT
  { forward :: StateT s m v
  , backward :: u -> WriterT (SubState context s) n v
  }

-- * Running

runForward :: forall c s m n u v. BiparserT c s m n u v -> s -> m (v, s)
runForward = runStateT . forward

evalForward :: forall c s m n u v. Functor m => BiparserT c s m n u v -> s -> m v
evalForward = (fmap fst .) . runForward

runBackward :: forall c s m n u v. BiparserT c s m n u v -> u -> n (v, SubState c s)
runBackward = (runWriterT .) . backward

execBackward :: forall c s m n u v. Functor n => BiparserT c s m n u v -> u -> n (SubState c s)
execBackward = (fmap snd .) . runBackward

-- * Mapping Forward

mapFW :: forall c s m n u v.
  Functor m
  => (v -> v)
  -> BiparserT c s m n u v
  -> BiparserT c s m n u v
mapFW f (BiparserT fw bw) = BiparserT (f <$> fw) bw

-- * Mapping Backward
-- Used to converte @u@ to the correct type for the biparser.

comap :: forall c s m n u u' v.
  (u -> u')
  -> BiparserT c s m n u' v
  -> BiparserT c s m n u v
comap f (BiparserT fw bw) = BiparserT fw (bw . f)

comapM :: forall c s m n u u' v.
  Monad n =>
  (u -> n u')
  -> BiparserT c s m n u' v
  -> BiparserT c s m n u v
comapM f (BiparserT fw bw) = BiparserT fw (\u -> WriterT $ f u >>= runWriterT . bw)

comapMay :: forall c s m n u u' v.
  ( Monoid (SubState c s)
  , Applicative n
  )
  => v
  -> (u -> Maybe u')
  -> BiparserT c s m n u' v
  -> BiparserT c s m n u  v
comapMay x f (BiparserT fw bw) = BiparserT fw $
  maybe (pure x) bw . f

infix 8 `upon`
upon :: forall c s m n u u' v.
  BiparserT c s m n u' v
  -> (u -> u')
  -> BiparserT c s m n u v
upon = flip comap

infix 8 `uponM`
uponM :: forall c s m n u u' v.
  Monad n
  => BiparserT c s m n u' v
  -> (u -> n u') -> BiparserT c s m n u v
uponM = flip comapM

infix 8 `uponMay`
uponMay :: forall c s m n u u' v.
  ( Monoid (SubState c s)
  , Applicative n
  )
  => BiparserT c s m n u' v
  -> v
  -> (u -> Maybe u')
  -> BiparserT c s m n u  v
uponMay x y z = comapMay y z x

-- * Map Backwards Write

infix 8 `mapBack`
mapBack :: forall c s m n u v ss.
  ( Functor n
  , ss ~ SubState c s
  )
  => BiparserT c s m n u v
  -> (ss -> ss)
  -> BiparserT c s m n u v
mapBack (BiparserT fw bw) f = BiparserT fw $ (mapWriterT $ fmap $ fmap f) . bw

-- * Constrained Subtypes
-- More constrained subtypes of BiparserT

-- | Easy way to create new simple Biparser with Profunctor dimap
identity :: (Monad m, Monad n, a ~ SubState c a) => Iso c m n a a
identity = BiparserT get (\u -> tell u $> u)

-- | Iso when @u ~ v@
type Iso c m n a b = BiparserT c a m n b b
--iso :: forall c m n a b.
--  ( Monad m
--  , Monad n
--  , a ~ SubState c a
--  )
--  => (a -> b)
--  -> (b -> a)
--  -> Iso c m n a b
--iso f g = dimap g f identity

-- ** Unit
-- Unit when @u@ and @v@ are @()@

type Unit c s m n = BiparserT c s m n () ()

-- | Throws away @u@ and @v@
unit :: forall c s m n u. Unit c s m n -> Const c s m n u
unit = comap $ const ()

-- ** Monomorphism
-- DEV NOTE: Unsure if this is the correct name.

mono :: forall c s m n a.
  ( Monad m
  , Monad n
  )
  => (a -> a)
  -> Iso c m n s a
  -> Iso c m n s a
mono f = dimap f f

-- ** Constant

-- | Discards @u@ and returns ()
type Const c s m n u = BiparserT c s m n u ()

-- | Discards @u@
type ConstU c s m n u v = BiparserT c s m n u v

-- | If forward succeds return @v@
-- If backward does not pass an equal @v@ then fail.
mkConst :: forall c s m n v.
  ( Eq v
  , Monad m
  , Monad n
  , Alternative n
  )
  => v
  -> Unit c s m n
  -> Iso c m n s v
mkConst x = ($> x) . comapM (bool empty (pure ()) . (== x))

-- * State Mapping

-- | Maps the state with an Iso
-- DEV NOTE: The context for the Iso and BiparserT should probably be different and the constraints @s ~ SubState c s@ and s@ ~ SubState c s@ should be removed.
mapState
  :: forall c s s' m.
   ( Monoid s
   , Monoid s'
   , Eq s'
   , Monad m
   , s ~ SubState c s
   , s' ~ SubState c s'
   )
  =>  Iso c m m s s'
  ->  BiparserT c s' m m
  :-> BiparserT c s  m m
mapState (BiparserT fw' bw') (BiparserT fw'' bw'') = BiparserT
  (   StateT
  $   runStateT fw'
  >=> \(ns,s) -> runStateT fw'' ns
  >>= \(x,ns') -> execWriterT (iterateUntilM (== mempty) bw' ns')
  >>= \s' -> pure (x, s' <> s)
  )
  (\u -> WriterT $ runWriterT (bw'' u) >>= \(x,w) -> runWriterT $ iterateUntilM (== mempty) bw' w $> x)

-- * Monad Mapping
-- Change the underlying monads.

mapMs :: forall c s m m' n n' u v.
  ( Traversable m
  , Functor m'
  )
  => (forall a. m a -> m' a)
  -> (forall a. n a -> n' a)
  -> BiparserT c s m n u v
  -> BiparserT c s m' n' u v
mapMs f g (BiparserT fw bw) = BiparserT
  (mapStateT f fw)
  (mapWriterT g . bw)

{-# WARNING mapMs' "Exposes the internals of BiparserT an you will probably use it incorrectly." #-} 
mapMs' ::
  ( Monoid (SubState c s)
  )
  => (forall s'. s' -> m (v,s') -> m' (v',s'))
  -> (forall w. w -> n (v,w) -> n' (v',w))
  -> BiparserT c s m n u v
  -> BiparserT c s m' n' u v'
mapMs' f g (BiparserT fw bw) = BiparserT
  (StateT \s -> f s $ runStateT fw s)
  (mapWriterT (g mempty) . bw)

fix :: forall c s m m' n n' u v.
  ( FixFail m
  , FixFail n
  , Monoid v
  , Monoid (SubState c s)
  , Applicative m'
  , Applicative n'
  )
  => BiparserT c s m n u v
  -> BiparserT c s m' n' u v
fix (BiparserT fw bw) = BiparserT
  (StateT \s -> pure $ (mempty, s) `fixFail` runStateT fw s)
  (\u -> WriterT . pure $ mempty `fixFail` runWriterT (bw u))

fixWith :: forall c s m n u v.
  ( FixFail m
  , FixFail n
  )
  => BiparserT c s Identity Identity u v
  -> BiparserT c s m n u v
  -> BiparserT c s Identity Identity u v
fixWith (BiparserT (runStateT -> fw) bw) (BiparserT (runStateT -> fw') bw') = BiparserT
  (StateT $ Identity . \s -> runIdentity (fw s) `fixFail` fw' s)
  (\u -> WriterT . Identity $ (runIdentity $ runWriterT $ bw u) `fixFail` runWriterT (bw' u))

class FixFail m where fixFail :: a -> m a -> a
instance FixFail Identity where fixFail _ = runIdentity
instance FixFail Maybe where fixFail = fromMaybe
instance FixFail (Either a) where fixFail = fromRight

-- * Forward and Backward Divergence

-- -- | Discards unused s' state to avoid commingling m and n monads.
-- --mapState'
-- --  :: forall c s s' m n.
-- --   ( MonadFail m
-- --   , Monad n
-- --   )
-- --  =>  Iso c m n s s'
-- --  ->  BiparserT c s' m n
-- --  :-> BiparserT c s  m n
-- --BiparserT fw' bw' `mapState'` BiparserT fw'' bw'' = BiparserT
-- --  (StateT $ runStateT fw' >=> \(s,s') -> runStateT fw'' s >>= \(x, _) -> pure (x, s'))
-- --  (\u -> WriterT $ runWriterT (bw'' u) >>= \(x,w) -> runWriterT (bw' w) >>= \(_,w') -> pure (x,w'))

-- | Use instead of 'empty' when only forward should fail but backward should continue
emptyForward :: forall c s m n u. (Monoid (SubState c s), MonadPlus m, Applicative n) => BiparserT c s m n u ()
emptyForward = BiparserT empty (const $ pure ())

-- * SubState
-- SubState allows for context outside of the parser and printer.

-- Line and column number for parsing error messages is an example of context that is important to maintain but annoying to directly deal with when writeing the parser.
type SubState :: Type -> Type -> Type
type family SubState context state

-- | Getter for the substate.
class GetSubState context state where
  getSubState :: state -> SubState context state

-- | Update the state's context and substate.
-- Used when more than one element at a time should be consumed and written.
-- This class is for effiencey and everything could be accomplished with 'UpdateStateWithElement' only.
-- UNKNOWN: Does 'UpdateStateWithSubState' need to be used in isolation from 'UpdateStateWithElement'
-- @state@ is the old state
-- The first @ss@ is the consumed substate
-- The second @ss@ is the new substate
-- Returns the updated state
class UpdateStateWithSubState context state where
  updateSubStateContext :: ss ~ SubState context state => state -> ss -> ss -> state

type SubStateContext context state = (GetSubState context state, UpdateStateWithSubState context state)

type SubElement :: Type -> Type -> Type
type SubElement c s = Element (SubState c s)

-- | Update the state's context and substate.
-- Used when only one element is consumed and written.
-- - @state@ is the old state
-- - @SubElement context state@ is the consumed element
-- - @SubState context state@ is the new substate
-- - Returns the updated state
class UpdateStateWithElement context state where
  updateElementContext :: state -> SubElement context state -> SubState context state -> state

type ElementContext context state = (GetSubState context state, UpdateStateWithElement context state)

-- ** Identity Context
-- Use as the context if @state ~ SubState IdentityStateContext state@ basically if there is no context outside the 

data IdentityStateContext
type instance SubState IdentityStateContext a = a
instance GetSubState IdentityStateContext state where
  getSubState = id

instance UpdateStateWithSubState IdentityStateContext state where
  updateSubStateContext _ _ s = s

instance UpdateStateWithElement IdentityStateContext state where
  updateElementContext _ _ s = s

instance (Functor m, Functor n) => Functor (BiparserT c s m n u) where
  fmap f (BiparserT fw' bw') = BiparserT
    (fmap f fw')
    (fmap f . bw')

-- * Atoms
-- Must be used to construct all other biparsers that have context.
-- Used to ensure that context is updated correctly.

-- | Takes and writes one element. Updates the context and substate.
one :: forall c s m n.
  ( IsSequence (SubState c s)
  , MonadFail m
  , Monad n
  , ElementContext c s
  ) => Iso c m n s (SubElement c s)
one = BiparserT fw bw
  where
  fw = do
    s <- get
    headTailMay (getSubState @c s) & \case
      Just (x, ss) -> put (updateElementContext @c s x ss) $> x
      Nothing -> fail "Could not take one element. The container is empty."
  bw :: SubElement c s -> WriterT (SubState c s) n (SubElement c s)
  bw c = tell (singleton c) $> c
  headTailMay :: IsSequence b => b -> Maybe (Element b, b)
  headTailMay x = do
    h <- headMay x
    s <- tailMay x
    return (h, s)

-- | Takes and writes substate. Updates the context and substate.
split :: forall c s m n ss.
  ( SubState c s ~ ss
  , SubStateContext c s
  , Monad m
  , Monad n
  )
  => (StateT ss m ss)
  -> Iso c m n s ss
split splitSubState = BiparserT fw bw
  where
  fw = do
    s <- get
    (start, end) <- lift $ runStateT splitSubState $ getSubState @c s
    put $ updateSubStateContext @c s start end
    return start
  bw :: ss -> WriterT ss n ss
  bw x = tell x $> x

-- | Modifies forward so that the Biparser does not modify the outside state.
peek :: forall c s m n u v.
  ( Monad m
  )
  => BiparserT c s m n u v
  -> BiparserT c s m n u v
peek (BiparserT fw bw) = BiparserT
  (get >>= \s -> fw <* put s)
  bw

-- | Allows trying a forward. If the forward fails the state is returned to the value it was before running.
-- ????? Unsure what should be done with the writer mondad
try :: MonadPlus m => BiparserT c s m n u v -> BiparserT c s m n u v
try (BiparserT fw bw) = BiparserT
  do
    s <- get
    fw <|> put s *> empty
  bw

optionalBack :: forall c s m n u u' v.
  ( Monoid (SubState c s)
  , Applicative n
  )
  => (u -> Maybe u')
  -> v
  -> BiparserT c s m n u' v
  -> BiparserT c s m n u  v
optionalBack f x (BiparserT fw bw) = BiparserT fw $ maybe (pure x) bw . f

instance (Monoid (SubState c s), Monad m, Applicative n) => Applicative (BiparserT c s m n u) where
  pure v = BiparserT (pure v) (const $ pure v)
  BiparserT fw' bw' <*> BiparserT fw'' bw'' = BiparserT
    (fw' <*> fw'')
    (\u -> bw' u <*> bw'' u)

instance (Monoid (SubState c s), MonadPlus m, Alternative n) => Alternative (BiparserT c s m n u) where
  empty = BiparserT empty (const empty)
  BiparserT fw' bw' <|> BiparserT fw'' bw'' =
    BiparserT (fw' <|> fw'') (\u -> bw' u <|> bw'' u)

instance (Monoid (SubState c s), Monad m, Monad n) => Monad (BiparserT c s m n u) where
  pu >>= kw = BiparserT fw' bw'
    where
    fw' = forward pu >>= forward . kw
    bw' u = backward pu u >>= ($ u) . backward . kw

instance (Monoid (SubState c s), MonadFail m, MonadFail n) => MonadFail (BiparserT c s m n u) where
  fail x = BiparserT (fail x) (const $ fail x)

instance (Functor m, Functor n) => Profunctor (BiparserT c s m n) where
  dimap f g (BiparserT fw' bw') = BiparserT (g <$> fw') (fmap g . bw' . f)

