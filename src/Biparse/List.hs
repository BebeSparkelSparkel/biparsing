{-# LANGUAGE PolyKinds #-}
module Biparse.List
  ( replicateBiparserT
  , takeElementsWhile
  , takeN
  , Many
  , many
  , manyId
  , manyIso
  , some
  , someIso
  , all
  , splitElem
  , splitOn
  , splitWith
  , whileM
  , whileM'
  , whileFwdAllBwd
  , untilFwdSuccessBwdAll
  --, whileId
  --, untilM
  --, untilM'
  --, untilId
  --, untilNothing
  , untilInclusive
  , untilExclusive
  , untilExclusive'
  , UntilClusive
  , untilClusive
  , intersperse
  , headAlt
  , tailAlt
  ) where

import Biparse.Biparser (Biparser(Biparser), forward, backward, Iso, SubElement, SubState, one, try, mono, ElementContext, FixFail, fix, peek, Unit, UpdateStateWithSubState, isNull, breakWhen', GetSubState, upon, uponM, UpdateStateWithElement, SubStateContext, forwardFail)
import Biparse.General (take, takeNot, memptyWrite, BreakWhen, rest, stripPrefix)
import Data.List.NonEmpty (NonEmpty, nonEmpty)
import Control.Profunctor.FwdBwd (MapMs(mapMs))
import Data.Maybe (fromJust)

replicateBiparserT :: forall c s m n u v.
  ( Monoid (SubState s)
  -- m
  , Monad m
  -- n
  , MonadFail n
  , Alt n
  )
  => Int
  -> Biparser c s m n u v
  -> Biparser c s m n [u] [v]
replicateBiparserT = \case
  0 -> const $ return mempty
  n -> \x -> do
    v <- x `uponM` (emptyFail n . headAlt)
    vs <- replicateBiparserT (n - 1) x `uponM` (emptyFail n . tailAlt)
    return $ v `cons` vs
  where
  emptyFail :: Int -> n a -> n a
  emptyFail n = (<!> (fail $ "Expected " <> show n <> " more elements but there are none left."))

-- | Takes
takeElementsWhile :: forall c s m n ss se e.
   ( MonadFail m
   , MonadState s m
   , MonadError e m
   , Alt m
   -- n
   , MonadWriter ss n
   , MonadFail n
   , Alt n
   -- substate
   , IsSequence ss
   -- context
   , ElementContext c s
   -- assignments
   , ss ~ SubState s
   , se ~ SubElement s
   )
  => (se -> Bool)
  -> Iso c m n s [se]
takeElementsWhile f =
  try do
    x <- one `uponM` headAlt
    unless (f x) forwardFail
    cons x <$> takeElementsWhile f `uponM` tailAlt
  <!> return mempty

-- | Take N elements
takeN :: forall c m n a ss se.
  -- m
  ( MonadFail m
  , MonadState a m
  , Alt m
  -- n
  , MonadWriter ss n
  , MonadFail n
  -- substate
  , IsSequence ss
  , GetSubState a
  , UpdateStateWithElement c a
  -- assignments
  , ss ~ SubState a
  , se ~ SubElement a
  )
  => Natural
  -> Iso c m n a [se]
takeN n = if n > 0
  then cons <$> one `uponM` headAlt <*> takeN (pred n) `uponM` tailAlt
  else pure mempty

type Many c s m n =
  ( Monoid (SubState s)
  , Monad m
  , Alt m
  , MonadFail n
  , Alt n
  )

-- | Applies given biparser zero or more times.
-- DEV QUESTION: Is it possible to use Alternative(many)?
many :: forall c s m n u v.
  Many c s m n
  => Biparser c s m n u v
  -> Biparser c s m n [u] [v]
many x =
  do
    y <- x `uponM` headAlt
    cons y <$> many x `uponM` tailAlt
  <!> pure mempty

manyId :: forall c s m n u v.
  ( FixFail m
  , Monad m
  , Alt m
  , FixFail n
  , Monoid (SubState s)
  , MonadFail n
  , Alt n
  )
  => Biparser c s m n u v
  -> Biparser c s Identity Identity [u] [v]
manyId = fix . many

-- | Iso version of 'many'
manyIso :: forall c s m n a.
  Many c s m n
  => Iso c m n s a
  -> Iso c m n s [a]
manyIso = many

type Some c s m n =
  ( Many c s m n
  , MonadFail m
  , MonadFail n
  )
-- | NonEmpty version of 'many'
some :: forall c s m n u v.
  Some c s m n
  => Biparser c s m n u v
  -> Biparser c s m n (NonEmpty u) (NonEmpty v)
some x = do
  ys <- many x `upon` toList
  maybe (fail "Expected to parse at least one but parsed none.") pure $ nonEmpty ys

-- | Iso version of 'some'
someIso :: forall c s m n a.
  Some c s m n
  => Iso c m n s a
  -> Iso c m n s (NonEmpty a)
someIso = some

-- | Consume all state till null. Any fail causes all to fail.
all :: forall c s m n u v ss e.
  -- m
  ( MonadState s m
  , MonadError e m
  -- n
  , MonadFail n
  -- substate
  , Monoid ss
  , MonoFoldable ss
  , GetSubState s
  , ss ~ SubState s
  )
  => Biparser c s m n u   v
  -> Biparser c s m n [u] [v]
all x = ifM isNull (pure mempty) do
  y <- x `uponM` headAlt
  cons y <$> all x `uponM` tailAlt

-- | Splits the substate on given element
splitElem :: forall c s m n ss se m' n'.
  -- m
  ( MonadState s m
  -- n
  , MonadWriter ss n
  -- substate
  , IsSequence ss
  , Eq se
  , Show se
  -- context
  , ElementContext c s
  -- assignments
  , m' ~ StateT s Maybe
  , n' ~ WriterT ss Maybe
  , se ~ SubElement s
  , ss ~ SubState s
  )
  => se
  -> Iso c m n s [ss]
splitElem x = liftStateMaybe $ correctEmpty splitter
  where
  splitter :: Iso c m' n' s [ss]
  splitter = do
    y <- fromList <$> manyIso (takeNot x) `uponM` fmap toList . headAlt
    take x *> (cons y <$> splitter `uponM` tailAlt) <!> pure (singleton y)
  correctEmpty :: Iso c m' n' s [ss] -> Iso c m' n' s [ss]
  correctEmpty = mono \case
    [y] | null y -> mempty
    [] -> [mempty]
    y -> y

splitOn :: forall c s m n ss m' n'.
  ( MonadState s m
  , MonadWriter ss n
  , Show ss
  , EqElement ss
  , ElementContext c s
  , SubStateContext c s
  , ss ~ SubState s
  , m' ~ StateT s Maybe
  , n' ~ WriterT ss Maybe
  )
  => ss
  -> Iso c m n s [ss]
splitOn = liftStateMaybe . splitWith @c @s @m' @n' . stripPrefix

liftStateMaybe :: forall c s m n u v ss.
  ( MonadState s m
  , MonadWriter ss n
  )
  => Biparser c s (StateT s Maybe) (WriterT ss Maybe) u v
  -> Biparser c s m n u v
liftStateMaybe = mapMs
 (\x -> do
   s <- get
   let (y,s') = fromJust $ runStateT x s
   put s'
   pure y
 )
 (\x -> do
   let (y,w) = fromJust $ runWriterT x
   tell w
   pure y
 )

splitWith :: forall c s m n ss e.
  ( BreakWhen c s m n ss e
  , UpdateStateWithSubState c s
  )
  => Unit c s m n
  -> Iso c m n s [ss]
splitWith x
  =   ifM isNull (pure mempty)
  $   do
        hs <- so `uponM` initAlt 
        l <- rest `uponM` lastAlt
        return $ hs `snoc` l
  <!> singleton <$> rest `upon` const mempty
  where
  so = ifM isNull
    (pure mempty)
    (breakWhen' x `uponM` headAlt ^:^ (so <!> pure mempty) `uponM` tailAlt)

-- | Runs 'predicate' and if 'predicate' returns 'True' then run 'produce'. Repeat until 'predicate' returns 'False'. The 'predicate' does not modify the state nor does it write.
whileM :: forall c s m n u v ss.
  ( MonadState s m
  , Alt m
  , MonadWriter ss n
  , MonadFail n
  , Alt n
  , ss ~ SubState s
  )
  => Biparser c s m n u Bool
  -> Biparser c s m n u v
  -> Biparser c s m n [u] [v]
whileM predicate produce = whileM' (peek $ memptyWrite predicate) produce

-- | Like 'whileM' but the predicate does modify the state and writes.
whileM' :: forall c s m n u v.
  ( Monad m
  , Alt m
  , MonadFail n
  , Alt n
  , Monoid (SubState s)
  )
  => Biparser c s m n u Bool
  -> Biparser c s m n u v
  -> Biparser c s m n [u] [v]
whileM' predicate produce = ifM (predicate `uponM` headAlt <!> pure False)
  (produce `uponM` headAlt ^:^ whileM' predicate produce `uponM` tailAlt)
  (pure mempty)

-- | For forward: runs 'predicate` first. If 'predicate' returns true, runs 'produce'.
-- For backward: 'produce' is run fore every 'u' in '[u]' and 'predicate' is run at the end.
whileFwdAllBwd :: forall c s m n u v.
  ( Monad m
  , Monad n
  )
  => Biparser c s m n () Bool
  -> Biparser c s m n u v
  -> Biparser c s m n [u] [v]
whileFwdAllBwd predicate produce = Biparser
  ( ifM (forward predicate)
    (cons <$> forward produce <*> forward (whileFwdAllBwd predicate produce))
    (pure mempty)
  )
  \us -> traverse (backward produce) us <* backward predicate ()

-- | Like 'whileFwdAllBwd' but runs 'produce' until 'predicate' succeeds.
untilFwdSuccessBwdAll :: forall c s m n u v.
  ( Monad m
  , Alt m
  , Monad n
  , Alt n
  )
  => Biparser c s m n u v
  -> Unit c s m n
  -> Biparser c s m n [u] [v]
untilFwdSuccessBwdAll produce predicate = whileFwdAllBwd (False <$ predicate <!> pure True) produce

-- | Should be able to use ghosts of departed prrofs to get rid of partial head tail
--whileId :: forall c s u v.
--  ( Monoid (SubState s)
--  )
--  => Biparser c s Identity Identity u Bool
--  -> Biparser c s Identity Identity u v
--  -> Biparser c s Identity Identity [u] [v]
--whileId p x = ifM (p `uponMay` False $ headMay)
--  ( x `upon` unsafeHead ^:^ whileId p x `upon` unsafeTail)
--  (pure mempty)

--untilM :: forall c s m n u v.
--  ( Monad n
--  , Monoid (SubState s)
--  )
--  => Biparser c s m n u Bool
--  -> Biparser c s m n u v
--  -> Biparser c s m n [u] [v]
--untilM = undefined
--
--untilM' :: forall c s m n u v.
--  , Monad n
--  , Monoid (SubState s)
--  )
--  => Biparser c s m n u Bool
--  -> Biparser c s m n u v
--  -> Biparser c s m n [u] [v]
--untilM' = undefined

-- | Should be able to use ghosts of departed prrofs to get rid of undefined
--untilId :: forall c s u v.
--  ( Monoid (SubState s)
--  )
--  => Biparser c s Identity Identity u Bool
--  -> Biparser c s Identity Identity u v
--  -> Biparser c s Identity Identity [u] [v]
--untilId = whileId . mapFW Data.Bool.not

---- Run 'bp' unitl it returns 'Nothing'. Fails if 'Nothing' is never returned.
--untilNothing :: forall c s m n u v.
--  UntilClusive c s m n
--  => Biparser c s m n u (Maybe v)
--  -> Biparser c s m n [u] [v]
--untilNothing bp
--  =    bp `uponM` headAlt
--  >>= maybe (pure mempty) \x -> cons x
--  <$> untilNothing bp `uponM` tailAlt

-- | Run 'bp' until 'p' succeeds. Includes the success result in list. If 'p' does not succeed, fails.
untilInclusive, untilExclusive :: forall c s m n u v.
  UntilClusive c s m n
  => (v -> Bool)
  -> Biparser c s m n u v
  -> Biparser c s m n [u] [v]
untilInclusive = untilClusive \f x -> f $ singleton x

-- | Like untilInclusive, but excludes the success result from the list,
untilExclusive = untilClusive \f _ -> f mempty

-- | Like 'untilExclusive' but includes the success result as the second value in the tuple.
untilExclusive' :: forall c s m n u v.
  UntilClusive c s m n
  => (v -> Bool)
  -> Biparser c s m n u v
  -> Biparser c s m n [u] ([v], v)
untilExclusive' = untilClusive \f x -> (f mempty, x)

type UntilClusive c s m n =
  ( Monoid (SubState s)
  , Monad m
  , MonadFail n
  )

-- | Builder for 'untilInclusive' and 'untilExclusive'.
untilClusive :: forall c s m n u v a.
  UntilClusive c s m n
  => (([v] -> [v]) -> v -> a)
  -> (v -> Bool)
  -> Biparser c s m n u v
  -> Biparser c s m n [u] a
untilClusive f p bp = uncurry f <$> uc
  where
  uc = do
    x <- bp `uponM` headAlt
    if p x
    then pure (id, x)
    else first (cons x .) <$> uc `uponM` tailAlt

intersperse :: forall c s m n u fu v fv v' ss e.
  -- m
  ( MonadState s m
  , MonadFail m
  , MonadError e m
  , Alt m
  -- n
  , MonadWriter ss n
  , MonadFail n
  , Alt n
  -- lists
  , MonoFoldable fu
  , Element fu ~ u
  , IsSequence fv
  , Element fv ~ v
  -- assignments
  -- , ss ~ SubState s
  )
  => Biparser c s m n u v
  -> (forall u'. Biparser c s m n u' v')
  -> Biparser c s m n fu fv
intersperse x y = fromList <$> intersperse' `upon` toList
  where 
  intersperse' =
    do
      x' <- x `uponM` headAlt
      xs <- try
        do
          _ <- y
          xs' <- intersperse' `uponM` tailAlt
          bool (pure xs') (fail "Could not intersperse between no elements.") $ null xs'
        <!> pure mempty
      pure $ cons x' xs
    <!> pure mempty

