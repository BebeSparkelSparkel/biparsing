{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE PatternSynonyms #-}
module Biparse.Biparser.Internal
  ( Biparser(Biparser, ..)
  , forward
  , setForward
  , backward
  , setBackward
  , Iso
  , IsoClass(..)
  , Unit
  , unit
  , mono
  , Const
  , ConstU
  , fix
  , FixFail(..)
  , comap
  , comapMay
  , comapEither
  , comapM
  , comapPred
  , upon
  , uponMay
  , uponEither
  , uponM
  , uponPred
  , mapWrite
  , emptyForward
  , ignoreForward
  , ignoreBackward
  , GetSubState(..)
  , UpdateStateWithSubState(..)
  , SubStateContext
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
  , count
  , ask'
  , asks'
  , resetState
  ) where

import Biparse.FixFail (FixFail(fixFail))
import Biparse.Utils (convertIntegralUnsafe)
import Control.Monad.Extra (findM)
import Control.Monad.Reader.Class (MonadReader(ask), asks)
import Control.Monad.Unrecoverable (MonadUnrecoverable, UnrecoverableError, throwUnrecoverable)
import Control.Monad.Writer.Class (listen)
import Control.Profunctor.FwdBwd (BwdMonad, Comap, FwdBwd, pattern FwdBwd, MapMs(mapMs), DualMap)
import Control.Profunctor.FwdBwd qualified as FB
import Data.Profunctor (Profunctor(dimap))

-- | Product type for simultainously constructing forward and backward running programs.
newtype Biparser context s m n u v = Biparser' {unBiparser :: FwdBwd m n u v}
pattern Biparser :: m v -> (u -> n v) -> Biparser c s m n u v
pattern Biparser fw bw = Biparser' (FwdBwd fw bw)
{-# COMPLETE Biparser #-}
forward :: forall c s m n u v. Biparser c s m n u v -> m v
forward (Biparser fw _) = fw
setForward :: forall c s m m' n u v. Biparser c s m n u v -> m' v -> Biparser c s m' n u v
setForward (Biparser _ bw) fw = Biparser fw bw
backward :: forall c s m n u v. Biparser c s m n u v -> u -> n v
backward (Biparser _ bw) = bw
setBackward :: forall c s m n n' u u' v. Biparser c s m n u v -> (u' -> n' v) -> Biparser c s m n' u' v
setBackward (Biparser fw _) = Biparser fw

type instance BwdMonad () (Biparser _ _ _ n) = n
deriving instance Monad n => Comap () (Biparser c s m n)
deriving instance (Functor m, Functor n) => DualMap (Biparser c s m n u)
deriving instance (MonadError e m, MonadError e n, Monoid (SubState s)) => MonadError e (Biparser c s m n u)
instance (MonadUnrecoverable m, MonadUnrecoverable n, UnrecoverableError m ~ UnrecoverableError n) => MonadUnrecoverable (Biparser c s m n u) where
  type UnrecoverableError (Biparser c s m n u) = UnrecoverableError (FwdBwd m n u)
  throwUnrecoverable = Biparser' . throwUnrecoverable

-- * Mapping Backward
-- Used to converte @u@ to the correct type for the biparser.

comap :: forall c s m n u u' v.
  Monad n
  => (u -> u')
  -> Biparser c s m n u' v
  -> Biparser c s m n u  v
comap = FB.comap @()

comapM :: forall c s m n u u' v.
  Monad n
  => (u -> n u')
  -> Biparser c s m n u' v
  -> Biparser c s m n u  v
comapM = FB.comapM @()

comapMay :: forall c s m n u u' v.
  ( Monad n
  , Alternative n
  )
  => (u -> Maybe u')
  -> Biparser c s m n u' v
  -> Biparser c s m n u  v
comapMay f (Biparser fw bw) = Biparser fw $ bw <=< maybe empty pure . f

comapEither :: forall c s m n u u' v.
  ( Applicative n
  )
  => (u -> Either v u')
  -> Biparser c s m n u' v
  -> Biparser c s m n u  v
comapEither f (Biparser fw bw) = Biparser fw $ either pure bw . f

comapPred :: forall c s m n u v.
  ( Monad n
  , Alternative n
  )
  => (u -> Bool)
  -> Biparser c s m n u v
  -> Biparser c s m n u v
comapPred p = comapM \u -> if p u then empty else pure u

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

infix 8 `uponMay`
uponMay :: forall c s m n u u' v.
  ( Monad n
  , Alternative n
  )
  => Biparser c s m n u' v
  -> (u -> Maybe u')
  -> Biparser c s m n u v
uponMay = flip comapMay

infix 8 `uponEither`
uponEither :: forall c s m n u u' v.
  ( Applicative n
  )
  => Biparser c s m n u' v
  -> (u -> Either v u')
  -> Biparser c s m n u  v
uponEither = flip comapEither

infix 8 `uponPred`
uponPred :: forall c s m n u v.
  ( Monad n
  , Alternative n
  )
  => Biparser c s m n u v
  -> (u -> Bool)
  -> Biparser c s m n u v
uponPred = flip comapPred

-- * Map Backwards Write

infix 8 `mapWrite`
mapWrite :: forall c s m n u v ss.
  MonadWriter ss n
  => Biparser c s m n u v
  -> (ss -> ss)
  -> Biparser c s m n u v
mapWrite (Biparser fw bw) f = Biparser fw $
  pass . fmap (,f) . bw

-- * Constrained Subtypes
-- More constrained subtypes of Biparser

-- | Iso when @u ~ v@
type Iso c m n a b = Biparser c a m n b b

class IsoClass c m n a b where iso :: Iso c m n a b

-- ** Unit
-- Unit when @u@ and @v@ are @()@

type Unit c s m n = Biparser c s m n () ()

-- | Throws away @u@ and @v@
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

-- * Monad Mapping
-- Change the underlying monads.

instance MapMs (Biparser c s) where
  mapMs f g (Biparser fw bw) = Biparser (f fw) (g . bw)

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
  (\u -> pure $ mempty `fixFail` bw u)

-- * Forward and Backward Divergence

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

-- | Getter for the substate.
class GetSubState state where
  type SubState state
  getSubState :: state -> SubState state



instance GetSubState (Identity s) where
  type SubState (Identity s) = s
  getSubState = runIdentity

-- | Update the state's context and substate.
-- Used when more than one element at a time should be consumed and written.
-- This class is for effiencey and everything could be accomplished with 'UpdateStateWithElement' only.
-- UNKNOWN: Does 'UpdateStateWithSubState' need to be used in isolation from 'UpdateStateWithElement'
-- @state@ is the old state
-- The first @ss@ is the consumed substate
-- The second @ss@ is the new substate
-- Returns the updated state
class UpdateStateWithSubState context state where
  updateSubStateContext :: ss ~ SubState state => state -> ss -> ss -> state

instance UpdateStateWithSubState c (Identity s) where updateSubStateContext _ _ = Identity

type SubStateContext context state = (GetSubState state, UpdateStateWithSubState context state)

type SubElement :: Type -> Type
type SubElement s = Element (SubState s)

-- | Update the state's context and substate.
-- Used when only one element is consumed and written.
-- - @state@ is the old state
-- - @SubElement state@ is the consumed element
-- - @SubState state@ is the new substate
-- - Returns the updated state
class UpdateStateWithElement context state where
  updateElementContext :: state -> SubElement state -> SubState state -> state

instance UpdateStateWithElement c (Identity s) where updateElementContext _ _ = Identity

type ElementContext context state = (GetSubState state, UpdateStateWithElement context state)

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
  , ss ~ SubState s
  )
-- | Takes and writes one element. Updates the context and substate.
one :: forall c s m n ss. One c s m n ss => Iso c m n s (SubElement s)
one = Biparser fw bw
  where
  fw = do
    s <- get
    (x, ss) <- headTailAlt (getSubState s) <|> fail "Could not take one element. The container is empty."
    put (updateElementContext @c s x ss) $> x
  bw :: SubElement s -> n (SubElement s)
  bw c = tell (singleton c) $> c

-- | Takes and writes substate. Updates the context and substate.
split :: forall c s m n ss.
  ( SubState s ~ ss
  , SubStateContext c s
  , MonadState s m
  , MonadWriter ss n
  )
  => StateT ss m ss
  -> Iso c m n s ss
split splitSubState = Biparser fw bw
  where
  fw = do
    s <- get
    (start, end) <- runStateT @ss splitSubState $ getSubState @s s
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
  , GetSubState s
  , MonadState s m
  , MonoFoldable ss
  , Applicative n
  , ss ~ SubState s
  )
  => Biparser c s m n u Bool
isNull = Biparser
  (gets @s $ null . getSubState)
  (pure . null)

write :: forall c s m n ss.
  ( Applicative m
  , MonadWriter ss n
  )
  => Biparser c s m n ss ()
write = Biparser (pure ()) tell

-- | 'x' does not succeed
breakWhen' :: forall c s m n ss.
  ( MonadState s m
  , Alternative m
  , MonadWriter ss n
  , SubStateContext c s
  , IsSequence ss
  , ss ~ SubState s
  )
  => Unit c s m n
  -> Iso c m n s ss
breakWhen' (Biparser fw bw) = Biparser fw' bw'
  where
  fw' = do
    startState <- get
    let its = initTails $ getSubState @s startState
    tryState $ maybe empty (pure . fst) =<< flip findM its \(h,t) -> do
      put $ updateSubStateContext @c startState h t
      fw $> True <|> pure False
  bw' x = do
    tell x
    bw ()
    return x

-- | Counts the number of elements consumed and written.
-- DEV NOTE: Sucky slow implementation.
count :: forall c s m n u v ss.
  -- m
  ( MonadState s m
  -- n
  , MonadWriter ss n
  -- substate
  , GetSubState s
  , MonoFoldable ss
  -- assignments
  , ss ~ SubState s
  )
  => Biparser c s m n u v
  -> Biparser c s m n u (Natural, v)
count (Biparser fw bw) = Biparser
  do
    s <- get
    x <- fw
    s' <- get
    let c = convertIntegralUnsafe $ length (getSubState @s s) - length (getSubState @s s')
    pure (c, x)
  \u -> do
    (x,w) <- listen @ss $ bw u
    pure (convertIntegralUnsafe $ length w, x)

-- | Return provided value forward and return reader value backward.
ask' :: (Applicative m, MonadReader r n) => r -> Biparser c s m n u r
ask' x = Biparser (pure x) (const ask)

-- | Return provided value forward and return the modified reader value backward.
asks' :: (Applicative m, MonadReader r n) => a -> (r -> a) -> Biparser c s m n u a
asks' x f = Biparser (pure x) (const $ asks f)

--ask'' :: (Monad n, Monoid (SubState s)) => M c s m r -> Biparser c s m n r u r
--ask'' = flip Biparser (const ask)

-- | Set the state as before if the predicate passes.
resetState :: forall c s m n u v.
  MonadState s m
  => (v -> Bool)
  -> Biparser c s m n u v
  -> Biparser c s m n u v
resetState p (Biparser fw bw) = Biparser
  do
    s <- get
    x <- fw
    when (p x) $ put s
    pure x
  bw

instance (Monoid (SubState s), Monad m, Applicative n) => Applicative (Biparser c s m n u) where
  pure v = Biparser (pure v) (const $ pure v)
  Biparser fw bw <*> Biparser fw' bw' = Biparser
    (fw <*> fw')
    (\u -> bw u <*> bw' u)

instance (Monoid (SubState s), MonadPlus m, Alternative n) => Alternative (Biparser c s m n u) where
  empty = Biparser empty (const empty)
  Biparser fw bw <|> Biparser fw' bw' =
    Biparser (fw <|> fw') (\u -> bw u <|> bw' u)

instance (Monoid (SubState s), Monad m, Monad n) => Monad (Biparser c s m n u) where
  pu >>= kw = Biparser fw bw
    where
    fw = forward pu >>= forward . kw
    bw u = backward pu u >>= ($ u) . backward . kw

instance (Monoid (SubState s), MonadFail m, MonadFail n) => MonadFail (Biparser c s m n u) where
  fail x = Biparser (fail x) (const $ fail x)

instance (Functor m, Functor n) => Profunctor (Biparser c s m n) where
  dimap f g (Biparser fw bw) = Biparser (g <$> fw) (fmap g . bw . f)

