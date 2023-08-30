{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE PatternSynonyms #-}
module Biparse.Biparser
  ( Biparser(Biparser, ..)
  , forward
  , setForward
  , backward
  , setBackward
  --, runForward
  --, evalForward
  --, runBackward
  --, execBackward
  , Iso
  , IsoClass(iso)
  --, iso
  , Unit
  , unit
  , mono
  , Const
  , ConstU
  --, mkConst
  , mapMs
  , fix
  --, fixWith
  , FixFail(..)
  , mapFW
  , comap
  , comapM
  --, comapMay
  , upon
  , uponM
  --, uponMay
  , mapWrite
  , emptyForward
  , ignoreForward
  , ignoreBackward
  , SubState
  , GetSubState(..)
  , UpdateStateWithSubState(..)
  , SubStateContext
  --, WrappedState(..)
  --, wrapState
  --, WrappedContext(..)
  --, wrapContext
  --, unwrapContext
  , SubElement
  , UpdateStateWithElement(..)
  , ElementContext
  , ReplaceSubState(..)
  , One
  , one
  , split
  , peek
  , try
  , optionalBack
  , isNull
  , write
  , breakWhen'
  , EmptyWrite
  , WriteConstructor
  , writeConstructor
  ) where

import Biparse.FixFail (FixFail(fixFail))
import Data.Profunctor (Profunctor(dimap))
import Control.Monad.Extra (findM)
import Control.Monad.ChangeMonad (ChangeMonad, ChangeFunction, changeMonad')
import Control.Monad.Writer.Class (listen)
import Control.Profunctor.FwdBwd (BwdMonad, Comap, FwdBwd, pattern FwdBwd)
import Control.Profunctor.FwdBwd qualified as FB
import Control.Monad.Writer (mapWriterT)

-- | Product type for simultainously constructing forward and backward running programs.
newtype Biparser context s m n u v = Biparser' {unBiparser :: FwdBwd m n u v}
pattern Biparser :: m v -> (u -> n v) -> Biparser c s m n u v
pattern Biparser fw bw = Biparser' (FwdBwd fw bw)
{-# COMPLETE Biparser #-}
forward :: Biparser c s m n u v -> m v
forward (Biparser fw _) = fw
setForward :: Biparser c s m n u v -> m' v -> Biparser c s m' n u v
setForward (Biparser _ bw) fw = Biparser fw bw
backward :: Biparser c s m n u v -> u -> n v
backward (Biparser _ bw) = bw
setBackward :: Biparser c s m n u v -> (u' -> n' v) -> Biparser c s m n' u' v
setBackward (Biparser fw _) bw = Biparser fw bw

type instance BwdMonad () (Biparser _ _ _ n) = n
deriving instance Monad n => Comap () (Biparser c s m n)
--
--data Biparser context s m n u v = Biparser
--  { forward :: m v
--  , backward :: u -> n v
--  }

-- * Running

--runForward :: forall c s m n u v. Biparser c s m n u v -> s -> m (v, s)
--runForward = runStateT . forward
--
--evalForward :: forall c s m n u v. Functor m => Biparser c s m n u v -> s -> m v
--evalForward = (fmap fst .) . runForward
--
--runBackward :: forall c s m n u v. Biparser c s m n u v -> u -> n (v, SubState c s)
--runBackward = (runWriterT .) . backward
--
--execBackward :: forall c s m n u v. Functor n => Biparser c s m n u v -> u -> n (SubState c s)
--execBackward = (fmap snd .) . runBackward

-- * Mapping Forward

mapFW :: forall c s m n u v.
  Functor m
  => (v -> v)
  -> Biparser c s m n u v
  -> Biparser c s m n u v
mapFW f (Biparser fw bw) = Biparser (f <$> fw) bw

-- * Mapping Backward
-- Used to converte @u@ to the correct type for the biparser.

comap :: forall c s m n u u' v.
  Monad n
  => (u -> u')
  -> Biparser c s m n u' v
  -> Biparser c s m n u v
comap = FB.comap @()

comapM :: forall c s m n u u' v.
  Monad n
  => (u -> n u')
  -> Biparser c s m n u' v
  -> Biparser c s m n u v
comapM = FB.comapM @()

--comapMay :: forall c s m n u u' v.
--  ( Applicative n
--  )
--  => v
--  -> (u -> Maybe u')
--  -> Biparser c s m n u' v
--  -> Biparser c s m n u  v
--comapMay x f (Biparser fw bw) = Biparser fw $
--  maybe (pure x) bw . f

infix 8 `upon`
upon :: forall c s m n u u' v.
  Monad n
  => Biparser c s m n u' v
  -> (u -> u')
  -> Biparser c s m n u v
upon = flip comap

infix 8 `uponM`
uponM :: forall c s m n u u' v.
  Monad n
  => Biparser c s m n u' v
  -> (u -> n u')
  -> Biparser c s m n u v
uponM = flip comapM

--infix 8 `uponMay`
--uponMay :: forall c s m n u u' v.
--  Applicative n
--  => Biparser c s m n u' v
--  -> v
--  -> (u -> Maybe u')
--  -> Biparser c s m n u  v
--uponMay x y z = comapMay y z x

-- * Map Backwards Write

infix 8 `mapWrite`
mapWrite :: forall c s m n u v ss.
  MonadWriter ss n
  => Biparser c s m n u v
  -> (ss -> ss)
  -> Biparser c s m n u v
mapWrite (Biparser fw bw) f = Biparser fw $
  --(mapWriterT $ fmap $ fmap f) . bw
  pass . fmap (,f) . bw

-- * Constrained Subtypes
-- More constrained subtypes of Biparser


-- | Iso when @u ~ v@
type Iso c m n a b = Biparser c a m n b b

class IsoClass c m n a b where iso :: Iso c m n a b

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

type Unit c s m n = Biparser c s m n () ()

-- | Throws away @u@ and @v@
--unit :: forall c s m n u. Unit c s m n -> Const c s m n u
unit :: forall c s m n u. Monad n => Unit c s m n -> Const c s m n u
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
type Const c s m n u = Biparser c s m n u ()

-- | Discards @u@
type ConstU c s m n u v = Biparser c s m n u v

-- | If forward succeds return @v@
-- If backward does not pass an equal @v@ then fail.
--mkConst :: forall c s m n v.
--  ( Eq v
--  , Monad m
--  , Monad n
--  , Alternative n
--  )
--  => v
--  -> Unit c s m n
--  -> Iso c m n s v
--mkConst x = ($> x) . comapM (bool empty (pure ()) . (== x))

-- * State Mapping

-- | Maps the state with an Iso
-- DEV NOTE: The context for the Iso and Biparser should probably be different and the constraints @s ~ SubState c s@ and s@ ~ SubState c s@ should be removed.
--mapState
--  :: forall c s s' m.
--   ( Monoid s
--   , Monoid s'
--   , Eq s'
--   , Monad m
--   , s ~ SubState c s
--   , s' ~ SubState c s'
--   )
--  =>  Iso c m m s s'
--  ->  Biparser c s' m m
--  :-> Biparser c s  m m
--mapState (Biparser fw' bw') (Biparser fw'' bw'') = Biparser
--  (   StateT
--  $   runStateT fw'
--  >=> \(ns,s) -> runStateT fw'' ns
--  >>= \(x,ns') -> execWriterT (iterateUntilM (== mempty) bw' ns')
--  >>= \s' -> pure (x, s' <> s)
--  )
--  (\u -> WriterT $ runWriterT (bw'' u) >>= \(x,w) -> runWriterT $ iterateUntilM (== mempty) bw' w $> x)

--switchContext :: forall c c' s m n.  Biparser c' s m n :-> Biparser c s m n
--switchContext = coerce

-- * Monad Mapping
-- Change the underlying monads.

mapMs :: forall c s s' m m' n n' u v
   . (forall a. m a -> m' a)
  -> (forall a. n a -> n' a)
  -> Biparser c s m n u v
  -> Biparser c s' m' n' u v
mapMs f g (Biparser fw bw) = Biparser (f fw) (g . bw)

-- {-# WARNING mapMs' "Exposes the internals of Biparser an you will probably use it incorrectly." #-} 
-- mapMs' ::
--   ( Monoid (SubState c s)
--   )
--   => (forall s'. s' -> m (v,s') -> m' (v',s'))
--   -> (forall w. w -> n (v,w) -> n' (v',w))
--   -> Biparser c s m n u v
--   -> Biparser c s m' n' u v'
-- mapMs' f g (Biparser fw bw) = Biparser
--   (StateT \s -> f s $ runStateT fw s)
--   (mapWriterT (g mempty) . bw)

fix :: forall c s m m' n n' u v.
  ( FixFail m
  , FixFail n
  , Monoid v
  , Applicative m'
  , Applicative n'
  )
  => Biparser c s m n u v
  -> Biparser c s m' n' u v
fix (Biparser fw bw) = Biparser
  (pure $ mempty `fixFail` fw)
  --(StateT \s -> pure $ (mempty, s) `fixFail` runStateT fw s)
  (\u -> pure $ mempty `fixFail` bw u)
  --(\u -> WriterT . pure $ mempty `fixFail` runWriterT (bw u))

--fixWith :: forall c s m n u v.
--  ( FixFail m
--  , FixFail n
--  )
--  => Biparser c s Identity Identity u v
--  -> Biparser c s m n u v
--  -> Biparser c s Identity Identity u v
--fixWith (Biparser (runStateT -> fw) bw) (Biparser (runStateT -> fw') bw') = Biparser
--  (StateT $ Identity . \s -> runIdentity (fw s) `fixFail` fw' s)
--  (\u -> WriterT . Identity $ (runIdentity $ runWriterT $ bw u) `fixFail` runWriterT (bw' u))

-- * Forward and Backward Divergence


---- | Discards unused s' state to avoid commingling m and n monads.
--mapState'
--  :: forall c s s' m n.
--   ( MonadFail m
--   , Monad n
--   )
--  =>  Iso c m n s s'
--  ->  Biparser c s' m n
--  :-> Biparser c s  m n
--mapState' (Biparser fw bw) (Biparser fw' bw') = Biparser
--  (StateT $ runStateT fw >=> \(s,s') -> runStateT fw' s >>= \(x, _) -> pure (x, s'))
--  (\u -> WriterT $ runWriterT (bw' u) >>= \(x,w) -> runWriterT (bw w) >>= \(_,w) -> pure (x,w))
-- | Use instead of 'empty' when only forward should fail but backward should continue
emptyForward :: forall c s m n u.
  ( MonadPlus m
  , Applicative n
  )
  => Biparser c s m n u ()
emptyForward = Biparser empty (const $ pure ())

-- | Throws away the forward computation and returns 'x'. Only the backwards computation runs.
ignoreForward :: forall c s m n u v.
  Applicative m
  => v
  -> Biparser c s m n u v
  -> Biparser c s m n u v
ignoreForward x y = setForward y $ pure x

ignoreBackward :: forall c s m n a.
  Applicative n
  => Iso c m n s a
  -> Iso c m n s a
ignoreBackward = flip setBackward pure

-- * SubState
-- SubState allows for context outside of the parser and printer.

-- Line and column number for parsing error messages is an example of context that is important to maintain but annoying to directly deal with when writeing the parser.
type SubState :: Type -> Type -> Type
type family SubState context state

-- | Getter for the substate.
class GetSubState context state where
  getSubState :: state -> SubState context state

--data WrappedState a s = WrappedState a s
--wrapState :: MapState m' m => Biparser c s m' n u v -> Biparser c (WrappedState a s) m n u v
--wrapState = mapState mapState'
--unwrapState :: Biparser c (WrappedState a s) m n u v -> Biparser c s m n u (a, v)
--unwrapState = mapState _
--newtype WrappedContext c = WrappedContext c
--wrapContext :: Biparser c s m n u v -> Biparser (WrappedContext c) s m n u v
--wrapContext = coerce
--unwrapContext :: Biparser (WrappedContext c) s m n u v -> Biparser c s m n u v
--unwrapContext = coerce
--type instance SubState (WrappedContext c) (WrappedState _ s) = SubState c s
--instance GetSubState c s => GetSubState (WrappedContext c) (WrappedState a s) where
--  getSubState (WrappedState _ x) = getSubState @c x

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


class ReplaceSubState s ss s' | s ss -> s' where replaceSubState :: s -> ss -> s'

instance (Functor m, Functor n) => Functor (Biparser c s m n u) where
  fmap f (Biparser fw' bw') = Biparser
    (fmap f fw')
    (fmap f . bw')

-- * Atoms
-- Must be used to construct all other biparsers that have context.
-- Used to ensure that context is updated correctly.

type One c s m n ss =
  ( IsSequence ss
  , ElementContext c s
  -- m
  , MonadState s m
  , MonadFail m
  , Alternative m
  -- n
  , MonadWriter ss n
  -- assignments
  , ss ~ SubState c s
  )
-- | Takes and writes one element. Updates the context and substate.
one :: forall c s m n ss.  One c s m n ss => Iso c m n s (SubElement c s)
one = Biparser fw bw
  where
  fw = do
    s <- get
    (x, ss) <- headTailAlt (getSubState @c s) <|> fail "Could not take one element. The container is empty."
    put (updateElementContext @c s x ss) $> x
  bw :: SubElement c s -> n (SubElement c s)
  bw c = tell (singleton c) $> c

-- | Takes and writes substate. Updates the context and substate.
split :: forall c s m n ss.
  ( SubState c s ~ ss
  , SubStateContext c s
  , MonadState s m
  , MonadWriter ss n
  )
  => (StateT ss m ss)
  -> Iso c m n s ss
split splitSubState = Biparser fw bw
  where
  fw = do
    s <- get
    (start, end) <- runStateT @ss splitSubState $ getSubState @c @s s
    put $ updateSubStateContext @c s start end
    return start
  bw :: ss -> n ss
  bw x = tell x $> x

-- | Modifies forward so that the Biparser does not modify the outside state.
peek :: forall c s m n u v.
  MonadState s m
  => Biparser c s m n u v
  -> Biparser c s m n u v
peek (Biparser fw bw) = Biparser
  (get @s >>= \s -> fw <* put s)
  bw

-- | Allows trying a forward. If the forward fails the state is returned to the value it was before running.
-- ????? Unsure what should be done with the writer mondad
try :: forall c s m n u v.
  ( MonadPlus m
  , MonadState s m
  )
  => Biparser c s m n u v
  -> Biparser c s m n u v
try (Biparser fw bw) = Biparser (tryState fw) bw

tryState :: forall s m v.
  ( Alternative m
  , MonadState s m
  )
  => m v
  -> m v
tryState fw = do
  s <- get @s
  fw <|> put s *> empty

-- | Allows back to not execute and return 'x' if 'f' returns 'Nothing'
optionalBack :: forall c s m n u u' v.
  Applicative n
  => (u -> Maybe u')
  -> v
  -> Biparser c s m n u' v
  -> Biparser c s m n u  v
optionalBack f x (Biparser fw bw) = Biparser fw $ maybe (pure x) bw . f

-- | Returns true if the substate is empty.
-- DEV NOTE: May be able to be written in general without Biparser constructor
isNull :: forall c s m n u ss.
  ( MonoFoldable u
  , GetSubState c s
  , MonadState s m
  , MonoFoldable ss
  , Applicative n
  , ss ~ SubState c s
  )
  => Biparser c s m n u Bool
isNull = Biparser
  (gets @s $ null . getSubState @c)
  (pure . null)

write :: forall c s m n ss.
  ( Applicative m
  , MonadWriter ss n
  )
  => Biparser c s m n ss ()
write = Biparser (pure ()) tell

-- | Like 'breakWhen' but fails if 'x' does not succeed
breakWhen' :: forall c s m n ss.
  ( MonadState s m
  , Alternative m
  , MonadWriter ss n
  , SubStateContext c s
  , IsSequence ss
  , ss ~ SubState c s
  )
  => Unit c s m n
  -> Iso c m n s ss
breakWhen' (Biparser fw bw) = Biparser fw' bw'
  where
  fw' = do
    startState <- get
    let its = initTails $ getSubState @c @s startState
    tryState $ maybe empty (pure . fst) =<< flip findM its \(h,t) -> do
      put $ updateSubStateContext @c startState h t
      fw $> True <|> pure False
  bw' x = do
    tell x
    bw ()
    return x

type WriteConstructor a b e n na nb ss m =
  ( MonadWriter a na
  , MonadWriter b nb
  , MonadWriter ss n
  , MonadError e n
  , ChangeMonad EmptyWrite na n
  , ChangeFunction EmptyWrite na n ~ ()
  , ChangeMonad EmptyWrite nb n
  , ChangeFunction EmptyWrite nb n ~ ()
  , Monad m
  )
data EmptyWrite
writeConstructor :: forall c s m n na nb a b e ss u v w.
  WriteConstructor a b e n na nb ss m
  => (a -> Either e (b -> Either e ss))
  -> Biparser c s m na u v
  -> (v -> Biparser c s m nb u w)
  -> Biparser c s m n u w
writeConstructor c (Biparser fw bw) bp = Biparser
  do
    x <- fw
    forward $ bp x
  \u -> do
    (x,x') <- changeMonad' @EmptyWrite () $ listen $ bw u
    c' <- either throwError pure $ c x'
    (y,y') <- changeMonad' @EmptyWrite () $ listen $ backward (bp x) u
    either throwError tell $ c' y'
    return y
instance (Monoid b, Monad n) => ChangeMonad EmptyWrite (WriterT a n) (WriterT b n) where
  type ChangeFunction EmptyWrite (WriterT a n) (WriterT b n) = ()
  changeMonad' () = mapWriterT (>>= \(x,_) -> pure (x,mempty))

instance (Monoid (SubState c s), Monad m, Applicative n) => Applicative (Biparser c s m n u) where
  pure v = Biparser (pure v) (const $ pure v)
  Biparser fw bw <*> Biparser fw' bw' = Biparser
    (fw <*> fw')
    (\u -> bw u <*> bw' u)

instance (Monoid (SubState c s), MonadPlus m, Alternative n) => Alternative (Biparser c s m n u) where
  empty = Biparser empty (const empty)
  Biparser fw bw <|> Biparser fw' bw' =
    Biparser (fw <|> fw') (\u -> bw u <|> bw' u)

instance (Monoid (SubState c s), Monad m, Monad n) => Monad (Biparser c s m n u) where
  pu >>= kw = Biparser fw bw
    where
    fw = forward pu >>= forward . kw
    bw u = backward pu u >>= ($ u) . backward . kw

instance (Monoid (SubState c s), MonadFail m, MonadFail n) => MonadFail (Biparser c s m n u) where
  fail x = Biparser (fail x) (const $ fail x)

instance (Functor m, Functor n) => Profunctor (Biparser c s m n) where
  dimap f g (Biparser fw bw) = Biparser (g <$> fw) (fmap g . bw . f)

