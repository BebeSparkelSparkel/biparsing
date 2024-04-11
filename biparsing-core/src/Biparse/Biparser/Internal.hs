{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE PatternSynonyms #-}
module Biparse.Biparser.Internal
  ( Biparser(..)
  , pattern Biparser
  , forward
  , setForward
  , backward
  , setBackward
  , GetContext
  , ForwardMonad
  , BackwardMonad
  , Iso
  , IsoClass(..)
  , coerceIso
  , Unit
  , unit
  , mono
  , Const
  , ConstU
  , comap
  , comapMay
  , comapEither
  , comapM
  , comapPred
  , comapPredM
  , comapConst
  , upon
  , uponMay
  , uponEither
  , uponM
  , uponPred
  , uponPredM
  , uponConst
  , mapWrite
  , onlyForwards
  , onlyBackwards
  , forwardFail
  , ignoreForward
  , ignoreBackward
  , GetSubState(..)
  , ReplaceSubState(..)
  , SubStateLens
  , subState
  , InitSuperState(..)
  , SuperArg
  , UpdateStateWithSubState(..)
  , SubStateContext
  , SubElement
  , UpdateStateWithElement(..)
  , ElementContext
  , UpdateStateWithNConsumed(..)
  , One
  , one
  , oneFw
  , split
  , splitFw
  , peek
  , try
  , optionalBack
  , isNull
  , eof
  , breakWhen'
  , count
  , askBw
  , asksBw
  , getBw
  , getsBw
  , resetState
  , ConvertElement(..)
  , ConvertSequence(..)
  ) where

import Biparse.Utils (headTailAlt)
import Lens.Micro (Lens, lens)
import Biparse.FixFail (FixFailM(fixFailM))
import Control.Applicative qualified
import Control.Monad.Extra (findM, whenM)
import Control.Monad.Unrecoverable (MonadUnrecoverable, UnrecoverableError, throwUnrecoverable)
import Control.Monad.Writer.Class (listen)
import Control.Profunctor.FwdBwd (BwdMonad, Comap, FwdBwd, pattern FwdBwd, MapMs(mapMs), DualMap)
import Control.Profunctor.FwdBwd qualified as FB
import Data.Either (Either)
import Data.Functor.Identity (runIdentity)
import Data.Profunctor (Profunctor(dimap))
import Data.Coerce (Coercible)
import Unsafe.Coerce (unsafeCoerce)

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
setBackward :: forall n c s m n' u u' v. Biparser c s m n u v -> (u' -> n' v) -> Biparser c s m n' u' v
setBackward (Biparser fw _) = Biparser fw

type instance BwdMonad () (Biparser _ _ _ n) = n

type GetContext :: Type -> Type
type family GetContext bp where
  GetContext (Biparser c _ _ _ _ _) = c
type ForwardMonad :: Type -> Type -> Type
type family ForwardMonad bp where
  ForwardMonad (Biparser _ _ m _ _ _) = m
  --ForwardMonad (Biparser _ _ m) = m
type BackwardMonad :: Type -> Type -> Type
type family BackwardMonad bp where
  BackwardMonad (Biparser _ _ _ n _ _) = n
  --BackwardMonad (Biparser _ _ _ n) = n

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
  MonadFail n
  => (u -> Maybe u')
  -> Biparser c s m n u' v
  -> Biparser c s m n u  v
comapMay f (Biparser fw bw) = Biparser fw $ bw <=< maybe (fail "backward map to Maybe gave Nothing.") pure . f

comapEither :: forall c s m n u u' v.
  Applicative n
  => (u -> Either v u')
  -> Biparser c s m n u' v
  -> Biparser c s m n u  v
comapEither f (Biparser fw bw) = Biparser fw $ either pure bw . f

comapPred :: forall c s m n u v.
  MonadFail n
  => (u -> Bool)
  -> Biparser c s m n u v
  -> Biparser c s m n u v
comapPred p = comapM \u -> if p u then (fail "backward predicate failed.") else pure u

comapPredM :: forall c s m n u v.
  MonadFail n
  => (u -> n Bool)
  -> Biparser c s m n u v
  -> Biparser c s m n u v
comapPredM p = comapM \u -> bool (fail "backward monadic predicate failed") (pure u) =<< p u

comapConst :: forall c s m n u u' v.
  Monad n
  => u'
  -> Biparser c s m n u' v
  -> Biparser c s m n u  v
comapConst = FB.comap @() . const

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
  MonadFail n
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
  MonadFail n
  => Biparser c s m n u v
  -> (u -> Bool)
  -> Biparser c s m n u v
uponPred = flip comapPred

infix 8 `uponPredM`
uponPredM :: forall c s m n u v.
  MonadFail n
  => Biparser c s m n u v
  -> (u -> n Bool)
  -> Biparser c s m n u v
uponPredM = flip comapPredM

infix 8 `uponConst`
uponConst :: forall c s m n u u' v.
  Monad n
  => Biparser c s m n u' v
  -> u'
  -> Biparser c s m n u  v
uponConst = flip comapConst

-- * Map Backwards Write

infix 8 `mapWrite`
mapWrite :: forall c s m n u v w.
  MonadWriter w n
  => Biparser c s m n u v
  -> (w -> w)
  -> Biparser c s m n u v
mapWrite (Biparser fw bw) f = Biparser fw $
  pass . fmap (,f) . bw

-- * Only One Direction

onlyForwards :: Applicative n => m () -> Const c s m n u
onlyForwards = flip Biparser $ const $ pure ()

-- | Only run a backwards operation
onlyBackwards :: Applicative m => (u -> n ()) -> Const c s m n u 
onlyBackwards = Biparser $ pure ()

-- * Constrained Subtypes
-- More constrained subtypes of Biparser

-- ** Iso

-- | Iso when @u ~ v@
type Iso c m n a b = Biparser c a m n b b

class IsoClass c m n a b where iso :: Iso c m n a b

coerceIso :: (Coercible b b', Coercible b' b) => Iso c m n a b -> Iso c m n a b'
coerceIso = unsafeCoerce

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

-- * Forward and Backward Divergence

forwardFail :: forall c s m n u.
  ( MonadFail m
  , Applicative n
  )
  => Biparser c s m n u ()
forwardFail = Biparser (fail "Purposely forward fail.") (const $ pure ())

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

-- | Getter and Setter for the substate.
class GetSubState state where
  type SubState state :: Type
  getSubState :: state -> SubState state

class SubState s' ~ ss' => ReplaceSubState s ss' s' | s ss' -> s' where replaceSubState :: s -> ss' -> s'

--class SubStateLens s s' ss ss' | s -> ss, s' -> ss', s ss' -> s' where
type SubStateLens s s' ss ss' = (GetSubState s, SubState s ~ ss, ReplaceSubState s ss' s')
subState :: SubStateLens s s' ss ss' => Lens s s' ss ss'
subState = lens getSubState replaceSubState

instance GetSubState (Identity s) where
  type SubState (Identity s) = s
  getSubState = runIdentity

-- * SuperState

type InitSuperState :: Type -> Type -> Constraint
class InitSuperState c ss where
  type SuperState c ss :: Type
  fromSubState :: SuperArg (SuperState c ss) -> ss -> SuperState c ss

type SuperArg :: Type -> Type
type family SuperArg s

-- * Updating State

-- | Update the state's context and substate.
-- Used when more than one element at a time should be consumed.
-- This class is for effiencey and everything could be accomplished with 'UpdateStateWithElement' only.
-- UNKNOWN: Does 'UpdateStateWithSubState' need to be used in isolation from 'UpdateStateWithElement'
-- @state@ is the old state
-- The first @ss@ is the consumed substate
-- The second @ss@ is the new substate
-- Returns the updated state
class UpdateStateWithSubState context state where
  updateSubStateContext :: ss ~ SubState state => state -> ss -> ss -> state

type SubStateContext context state = (GetSubState state, UpdateStateWithSubState context state)

type SubElement :: Type -> Type
type SubElement s = Element (SubState s)

-- | Update the state's context and substate.
-- Used when only one element is consumed.
-- - @state@ is the old state
-- - @SubElement state@ is the consumed element
-- - @SubState state@ is the new substate
-- - Returns the updated state
class UpdateStateWithElement context state where
  updateElementContext :: state -> SubElement state -> SubState state -> state

type ElementContext context state = (GetSubState state, UpdateStateWithElement context state)

-- | Update the state's context and substate.
-- - @state@ is the old state
-- - @Index (SubState state)@ is the number of elements consumed
-- - Returns the updated state
class UpdateStateWithNConsumed context state where
  updateStateWithNConsumed :: state -> Index (SubState state) -> state

instance (Functor m, Functor n) => Functor (Biparser c s m n u) where
  fmap f (Biparser fw' bw') = Biparser
    (fmap f fw')
    (fmap f . bw')

-- * Atoms
-- Must be used to construct all other biparsers that have context.
-- Used to ensure that context is updated correctly.

type One c s m n ss se w =
  ( IsSequence ss
  , ElementContext c s
  -- m
  , MonadState s m
  , MonadFail m
  , Alt m
  -- n
  , MonadWriter w n
  , ConvertElement c se w n
  -- w
  -- assignments
  , ss ~ SubState s
  , se ~ SubElement s
  )
-- | Takes and writes one element. Updates the context and substate.
one :: forall w c s m n ss se. One c s m n ss se w => Iso c m n s se
one = Biparser (oneFw @c) bw
  where
  bw :: se -> n se
  bw c = (tell =<< convertElement @c c) $> c

-- | Forward Only! Takes one element. Updates the context and substate.
-- Useful for when forwards and backwards are to divergent to reasonably work with
-- and you still want to correctly update the state.
oneFw :: forall c s m ss se.
  ( IsSequence ss
  , ElementContext c s
  -- m
  , MonadState s m
  , MonadFail m
  , Alt m
  -- assignments
  , ss ~ SubState s
  , se ~ SubElement s
  ) => m se
oneFw = do
  s <- get
  (x, ss) <- headTailAlt (getSubState s) <!> fail "Could not take one element. The container is empty."
  put (updateElementContext @c s x ss) $> x

-- | Takes and writes substate. Updates the context and substate.
split :: forall c s m n ss w.
  ( SubStateContext c s
  , MonadState s m
  , MonadWriter w n
  , ConvertSequence c ss w n
  , SelectableStateT c
  , ss ~ SubState s
  )
  => StateTransformer c ss m ss
  -> Iso c m n s ss
split splitSubState = Biparser fw bw
  where
  fw = do
    s <- get
    (start, end) <- runStateT @c @ss splitSubState $ getSubState @s s
    put $ updateSubStateContext @c s start end
    return start
  bw :: ss -> n ss
  bw x = (tell =<< convertSequence @c x) $> x

-- | Takes and writes substate. Updates the context and substate.
splitFw :: forall c s m n ss u.
  ( MonadState s m
  , Applicative n
  , SubStateContext c s
  , SelectableStateT c
  , ss ~ SubState s
  )
  => StateTransformer c ss m ss
  -> Const c s m n u
splitFw splitSubState = Biparser
  do
    s <- get
    (start, end) <- runStateT @c @ss splitSubState $ getSubState @s s
    put $ updateSubStateContext @c s start end
 $ const $ pure ()

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
try :: forall c s m n u v e.
  ( MonadError e m
  , MonadState s m
  )
  => Biparser c s m n u v
  -> Biparser c s m n u v
try (Biparser fw bw) = Biparser (tryState fw) bw

tryState :: forall s m v e.
  ( MonadState s m
  , MonadError e m
  )
  => m v
  -> m v
tryState fw = do
  s <- get @s
  catchError fw \e -> put s *> throwError e

-- | Allows back to not execute and return 'x' if 'f' returns 'Nothing'
optionalBack :: forall c s m n u u' v.
  Applicative n
  => (u -> Maybe u')
  -> v
  -> Biparser c s m n u' v
  -> Biparser c s m n u  v
optionalBack f x (Biparser fw bw) = Biparser fw $ maybe (pure x) bw . f

-- * End Dectection

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
isNull = Biparser subStateNull (pure . null)

-- fails if not at the end of the input
eof ::
  ( MonadState s m
  , MonadFail m
  , Applicative n
  , GetSubState s
  , MonoFoldable (SubState s)
  ) => Const c s m n u
eof = Biparser
  (whenM subStateNull (fail "Expected to be at the end of input but there is still more"))
  (const $ pure ())

subStateNull :: (MonadState s m, GetSubState s, MonoFoldable (SubState s)) => m Bool
subStateNull = gets $ null . getSubState

-- | 'x' does not succeed
breakWhen' :: forall c s m n ss w e.
  ( MonadState s m
  , Alt m
  , MonadError e m
  , MonadFail m
  , MonadWriter w n
  , ConvertSequence c ss w n
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
    tryState $ maybe (fail "Could not find break.") (pure . fst) =<< flip findM its \(h,t) -> do
      put $ updateSubStateContext @c startState h t
      fw $> True <!> pure False
  bw' x = do
    tell =<< convertSequence @c x
    bw ()
    return x

-- | Counts the number of elements consumed and written.
-- DEV NOTE: Sucky slow implementation.
count :: forall c s m n u v ss w.
  -- m
  ( MonadState s m
  -- n
  , MonadWriter w n
  -- w
  , MonoFoldable w
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
    let c = fromIntegral $ length (getSubState @s s) - length (getSubState @s s')
    pure (c, x)
  \u -> do
    (x,w) <- listen $ bw u
    pure (fromIntegral $ length w, x)

-- * Backwards Reader and State

-- | Return provided value forward and return the reader value backward.
askBw :: (Applicative m, MonadReader r n) => r -> Biparser c s m n u r
askBw x = Biparser (pure x) (const ask)

-- | Return provided value forward and return the modified reader value backward.
asksBw :: (Applicative m, MonadReader r n) => a -> (r -> a) -> Biparser c s m n u a
asksBw x f = Biparser (pure x) (const $ asks f)

-- | Return provided value forward and return the state value backward.
getBw :: (Applicative m, MonadState ws n) => ws -> Biparser c s m n u ws
getBw x = Biparser (pure x) (const $ get)

-- | Return provided value forward and return the modified state value backward.
getsBw :: (Applicative m, MonadState ws n) => a -> (ws -> a) -> Biparser c s m n u a
getsBw x f = Biparser (pure x) (const $ gets f)

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

-- * Biparser Instances

deriving instance Monad n => Comap () (Biparser c s m n)

deriving instance (Functor m, Functor n) => DualMap (Biparser c s m n u)

deriving instance (MonadError e m, MonadError e n) => MonadError e (Biparser c s m n u)

instance (MonadUnrecoverable m, MonadUnrecoverable n, UnrecoverableError m ~ UnrecoverableError n) => MonadUnrecoverable (Biparser c s m n u) where
  type UnrecoverableError (Biparser c s m n u) = UnrecoverableError (FwdBwd m n u)
  throwUnrecoverable = Biparser' . throwUnrecoverable

instance (Applicative m, Applicative n) => Applicative (Biparser c s m n u) where
  pure v = Biparser (pure v) (const $ pure v)
  Biparser fw bw <*> Biparser fw' bw' = Biparser
    (fw <*> fw')
    (\u -> bw u <*> bw' u)

instance (Control.Applicative.Alternative m, Control.Applicative.Alternative n) => Control.Applicative.Alternative (Biparser c s m n u) where
  empty = Biparser Control.Applicative.empty (const Control.Applicative.empty)
  Biparser fw bw <|> Biparser fw' bw' =
    Biparser (fw Control.Applicative.<|> fw') (\u -> bw u Control.Applicative.<|> bw' u)

instance (Alt m, Alt n) => Alt (Biparser c s m n u) where
  Biparser fw bw <!> Biparser fw' bw' =
    Biparser (fw <!> fw') (\u -> bw u <!> bw' u)

instance (Monad m, Monad n) => Monad (Biparser c s m n u) where
  pu >>= kw = Biparser fw bw
    where
    fw = forward pu >>= forward . kw
    bw u = backward pu u >>= ($ u) . backward . kw

instance (MonadFail m, MonadFail n) => MonadFail (Biparser c s m n u) where
  fail x = Biparser (fail x) (const $ fail x)

instance (Functor m, Functor n) => Profunctor (Biparser c s m n) where
  dimap f g (Biparser fw bw) = Biparser (g <$> fw) (fmap g . bw . f)

instance (FixFailM m m', FixFailM n n') => FixFailM (Biparser c s m n u) (Biparser c s m' n' u) where
  fixFailM x (Biparser fw bw) = Biparser (fixFailM x fw) (fixFailM x . bw)

