module Biparse.General
  ( identity
  , Take
  , take
  , takeUnit
  , takeUni
  , takeDi
  , takeTri
  , takeNot
  , takeWhile
  , Take'
  , takeDi'
  , takeTri'
  , drop
  , DropWhile
  , dropWhile
  , dropUntil
  , skipUntil
  , untilJust
  , takeN
  , Pad
  , pad
  , padSet
  , padCount
  , BreakWhen
  , breakWhen
  , breakAt
  , optionMaybe
  , optional
  , stripPrefix
  , countElement
  , countElementSome
  , FromNatural(..)
  , not
  , failBool
  , memptyWrite
  , rest
  , failForward
  , failBackward
  , shouldFail
  ) where

import Data.Sequences qualified as MT
import Control.Profunctor.FwdBwd (endoSecond)
import Biparse.Biparser (Biparser, pattern Biparser, Iso, Unit, unit, one, oneFw, try, SubState, SubElement, ElementContext, SubStateContext, split, Const, mapWrite, Unit, upon, uponM, comapM, count, resetState, splitFw, ignoreBackward, peek)
import Data.Bool qualified
import Data.EqElement qualified
import Control.Profunctor.FwdBwd (firstM)
import Data.Set (Set, member)

identity :: forall c s m n ss w.
  ( MonadState s m
  , MonadWriter w n
  , Monoid ss
  , ConvertSequence c ss w n
  , SubStateContext c s
  , ContextualStateTransformerPLEASEREMOVESUFFIX c ss m
  , ss ~ SubState s
  )
  => Iso c m n s ss
identity = split do
  x <- get
  put mempty
  return x

-- * Take for single elements

type Take c s m n ss se w e =
  -- m
  ( MonadState s m
  , MonadFail m
  , MonadError e m
  , Alt m
  -- n
  , MonadWriter w n
  , MonadFail n
  -- context
  , ElementContext c s
  -- substate
  , Show (SubElement s)
  , Eq (SubElement s)
  , IsSequence (SubState s)
  -- w
  , ConvertElement c se w n
  -- assignments
  , ss ~ SubState s
  , se ~ SubElement s
  )

-- | Assumes but disregards the writer context
take :: forall c s m n u ss se w e. Take c s m n ss se w e => se -> Const c s m n u
take takeWrite = unit $ takeUnit takeWrite

-- | Discards the match
takeUnit :: forall c s m n ss se w e. Take c s m n ss se w e => se -> Unit c s m n
takeUnit takeWrite = void $ takeUni takeWrite `upon` const takeWrite

-- | Returns the match
takeUni :: forall c s m n ss se w e.
  Take c s m n ss se w e
  => se
  -> Iso c m n s se
takeUni takeWriteMatchReturn = try do
  y <- one
  unless (takeWriteMatchReturn == y) $ expectedFail takeWriteMatchReturn y
  return y

takeDi :: forall c s m n u ss se w e.
  ( Take c s m n ss se w e
  , Eq u
  , Show u
  )
  => se
  -> u
  -> Iso c m n s u
takeDi takeWrite matchReturn = takeTri takeWrite matchReturn matchReturn

-- | Allows 'SubElement s'`, 'u', and 'v' to be fixed which works well with Alternative.
takeTri :: forall c s m n u v ss se w e.
  ( Take c s m n ss se w e
  , Eq u
  , Show u
  )
  => se
  -> u
  -> v
  -> Biparser c s m n u v
takeTri takeWrite toMatch toReturn = try do
  x <- one `uponM` ($> takeWrite) . \x -> bool (expectedFail toMatch x) (pure ()) $ x == toMatch
  unless (takeWrite == x) $ expectedFail takeWrite x
  return toReturn

expectedFail :: (MonadFail m, Show a, Show b) => a -> b -> m ()
expectedFail x y = fail $ "Expected a " <> show x <> " but received a " <> show y

takeNot :: forall c s m n ss se w e.
  Take c s m n ss se w e
  => se
  -> Iso c m n s se
takeNot x = try do
  y <- one
  when (x == y) $ fail $ "Should not have found an " <> show y
  return y

-- * Take for prefixes

type Take' c s m n ss se w u e =
  -- m
  ( MonadState s m
  , MonadFail m
  , MonadError e m
  , Alt m
  -- n
  , MonadWriter w n
  , MonadFail n
  -- substate
  , Show ss
  , EqElement ss
  , ConvertSequence c ss w n
  -- context
  , SubStateContext c s
  -- u
  , Eq u
  , Show u
  -- transformer
  , SelectableStateT c
  -- assignments
  , ss ~ SubState s
  , se ~ SubElement s
  )

takeDi' :: forall c s m n u ss se w e.
  Take' c s m n ss se w u e
  => ss
  -> u
  -> Iso c m n s u
takeDi' takeWrite matchReturn = takeTri' takeWrite matchReturn matchReturn

-- | Allows 'SubState c s'`, 'u', and 'v' to be fixed which works well with Alternative.
takeTri' :: forall c s m n u v ss se w e.
  Take' c s m n ss se w u e
  => ss
  -> u
  -> v
  -> Biparser c s m n u v
takeTri' takeWrite toMatch toReturn = try do
  stripPrefix takeWrite `uponM` \x -> bool (expectedFail toMatch x) (pure ()) $ x == toMatch
  return toReturn

-- * Take while predicate

type TakeWhile c s m n ss se w =
  ( SubStateContext c s
  , IsSequence ss
  , MonadState s m
  , MonadWriter w n
  , ConvertSequence c ss w n
  , SelectableStateT c
  , ss ~ SubState s
  , se ~ SubElement s
  )

takeWhile :: forall c s m n ss se w.
  TakeWhile c s m n ss se w
  => (se -> Bool)
  -> Iso c m n s (SubState s)
takeWhile = split . state @c . span

drop :: forall c s m n u v e.
  ( MonadState s m
  , MonadError e m
  , Alt m
  , Monad n
  , Alt n
  )
  => Biparser c s m n u v
  -> Biparser c s m n u ()
drop bp = ignoreBackward () $ try bp *> drop bp <!> pure ()

type DropWhile c s m n u ss se w =
  ( SubStateContext c s
  , IsSequence ss
  , MonadState s m
  , MonadWriter w n
  , SelectableStateT c
  , ss ~ SubState s
  , se ~ SubElement s
  )

-- | Drop forward elements while predicate is true.
dropWhile :: forall c s m n u ss se w.
  DropWhile c s m n u ss se w
  => (se -> Bool)
  -> Const c s m n u
dropWhile f = splitFw $ stateT @c $ return . MT.span f

-- | Drop forward elements until @se@ is found. Fails if @se@ is not found.
dropUntil :: forall c s m n u ss se w e.
  ( DropWhile c s m n u ss se w
  , Take c s m n ss se w e
  )
  => se
  -> Const c s m n u
dropUntil x = dropWhile (/= x) <* take x

-- | Run until returns True
skipUntil :: forall c s m n u.
  ( MonadState s m
  , Monad n
  , Monoid (SubState s)
  )
  => Biparser c s m n u Bool
  -> Const c s m n u
skipUntil x = bool (void $ skipUntil x) (pure ()) =<< resetState id x

-- | Run until Just
untilJust :: forall c s m n u a ss.
  ( Monad m
  , Monad n
  , Monoid ss
  , ss ~ SubState s
  )
  => Biparser c s m n u (Maybe a)
  -> Biparser c s m n u a
untilJust x = maybe (untilJust x) pure =<< x

-- * N elements

-- | Take n elements. Does not limit what is written backwards.
takeN :: forall c s m n ss w.
  ( MonadState s m
  , MonadWriter w n
  , SubStateContext c s
  , IsSequence ss
  , ConvertSequence c ss w n
  , SelectableStateT c
  , ss ~ SubState s
  )
  => Index ss
  -> Iso c m n s ss
takeN = split . stateT @c . (return .) . MT.splitAt

-- * Pad

type Pad c s m n u v ss i se w j =
  -- m
  ( MonadState s m
  -- n
  , MonadWriter w n
  -- substate
  , IsSequence ss
  , SubStateContext c s
  , Eq se
  , Ord i
  , Num i
  , ConvertSequence c ss w n
  -- w
  , IsSequence w
  , Element w ~ se
  , Num j
  , Ord j
  -- transformer
  , SelectableStateT c
  -- Assignments
  , ss ~ SubState s
  , se ~ SubElement s
  , i ~ Index ss
  , j ~ Index w
  )

-- | Consumes the pad 'c' charcaters forward. Prepends the pad 'c' caracters backwards to ensure there are 'n' charcaters written.
-- Probably best to roll your own if using a writer type like 'String' that has as slow length function.
pad :: forall c s m n u v ss i se w j.
  Pad c s m n u v ss i se w j
  => Index w
  -> se
  -> Biparser c s m n u v
  -> Biparser c s m n u v
pad n c = padTemplate (== c) n c

padSet :: forall c s m n u v ss i se w j.
  ( Pad c s m n u v ss i se w j
  , Ord se
  )
  => Index w
  -> se
  -> Set se
  -> Biparser c s m n u v
  -> Biparser c s m n u v
padSet n c cs = padTemplate (`member` cs) n c

padTemplate :: forall c s m n u v ss i se w j.
  Pad c s m n u v ss i se w j
  => (se -> Bool)
  -> Index w
  -> se
  -> Biparser c s m n u v
  -> Biparser c s m n u v
padTemplate dropPred n c x = do
  dropWhile dropPred
  mapWrite x \y ->
    let l = lengthIndex y
    in if l >= n
      then y
      else replicate (n - l) c <> y

-- | Gives the pad count found for forward (number of c + number consumed by x). Just returns n backwards.
padCount :: forall c s m n u v ss i se w j.
  Pad c s m n u v ss i se w j
  => Index w
  -> se
  -> Biparser c s m n u v
  -> Biparser c s m n u (Natural, v)
padCount n c x = endoSecond (first $ const $ fromIntegral n) $ count $ pad n c x

-- * Break

type BreakWhen c s m n ss se w e =
  -- m
  ( MonadState s m
  , MonadFail m
  , MonadError e m
  , Alt m
  -- n
  , MonadWriter w n
  , MonadFail n
  , Alt n
  -- substate
  , IsSequence ss
  -- w
  , ConvertSequence c ss w n
  -- context
  , ElementContext c s
  -- assignments
  , ss ~ SubState s
  , se ~ SubElement s
  )

-- | Breaks off the substate head when 'x' succeeds. Writes x after given 'ss'.
-- DEV NOTE: Seems like there could be a more simple solution.
breakWhen :: forall c s m n ss se w e.
  BreakWhen c s m n ss se w e
  => Unit c s m n
  -> Iso c m n s ss
breakWhen (Biparser fw bw) = Biparser
  fw'
  \u -> do
    tell =<< convertSequence @c u
    bw ()
    return u
  where
  fw' = mempty <$ fw <!> cons <$> oneFw @c <*> fw'
    

breakAt :: forall c s m n ss se w e.
  ( BreakWhen c s m n ss se w e
  , ConvertElement c se w n
  , Eq se
  , Show se
  )
  => se
  -> Iso c m n s ss
breakAt = breakWhen . take

---- | Like 'breakWhen' but fails if 'x' does not succeed
--breakWhen' :: forall c s m n ss.
--  BreakWhen c s m n ss
--  => Unit c s m n
--  -> Iso c m n s ss
--breakWhen' x
--  = bw
--  <!> ignoreForward  (write *> unit x)
--  where
--  bw
--    =   mempty <$ (failBackward $ try $ unit x)
--    <!> do
--          y <- one `uponM` headAlt
--          cons y <$> breakWhen' x `uponM` tailAlt

-- | Consumes rest/all of substate and writes given
rest :: forall c s m n ss w.
  ( MonadState s m
  , MonadWriter w n
  , SubStateContext c s
  -- substate
  , Monoid ss
  , ConvertSequence c ss w n
  , ContextualStateTransformerPLEASEREMOVESUFFIX c ss m
  -- assignments
  , ss ~ SubState s
  )
  => Iso c m n s ss
rest = split $ get <* put mempty

-- * failure

failForward :: forall c s m n u v.
  MonadFail m
  => Biparser c s m n u v
  -> Biparser c s m n u v
failForward = firstM $ const $ fail "pureposely failed in the forwards direction"

failBackward :: forall c s m n u v.
  MonadFail n
  => Biparser c s m n u v
  -> Biparser c s m n u v
failBackward = comapM $ const $ fail "pureposely failed in the backwards direction"

-- | The forward of the given biparser should fail. If it does not fail, fail with the given string.
-- Backwards is ignored.
shouldFail :: forall c s m n u u' v.
  ( MonadFail m
  , MonadState s m
  , Alt m
  , MonadFail n
  , Alt n
  )
  => Biparser c s m n u v
  -> String
  -> Biparser c s m n u' ()
shouldFail bp msg = ignoreBackward () $ peek $ bool (pure ()) (fail msg) =<< failBool bp

-- * Optional parsing

optionMaybe :: forall c s m n u v e.
  ( MonadState s m
  , MonadError e m
  , Alt m
  , MonadFail n
  , Alt n
  )
  => Biparser c s m n u v
  -> Biparser c s m n u (Maybe v)
optionMaybe x = Just <$> try x <!> pure Nothing

-- | Allows a parser to fail and return Maybe instead. Allows writer to optionally run or not.
optional :: forall c s m n u v e.
  ( MonadFail m
  , Alt m
  , MonadError e m
  , MonadState s m
  , MonadFail n
  , Alt n
  )
  => Biparser c s m n u v
  -> Biparser c s m n (Maybe u) (Maybe v)
optional x = Just <$> try x `uponM` maybe (fail "") pure <!> pure Nothing

-- * Stripping

stripPrefix :: forall c s m n ss w u.
  ( EqElement ss
  , SubStateContext c s
  , Show ss
  , MonadState s m
  , MonadFail m
  , MonadWriter w n
  , ConvertSequence c ss w n
  , SelectableStateT c
  , ss ~ SubState s
  )
  => ss
  -> Const c s m n u
stripPrefix pre = unit $ void s `upon` const pre
  where
  s :: Iso c m n s ss
  s = split $ stateT @c
    $ maybe
      (fail $ "Could not strip prefix: " <> show pre)
      (pure . (pre,))
    . Data.EqElement.stripPrefix pre

-- * Counting

-- | Counts 0 or more elements
countElement :: forall c s m n ss se w.
  ( FromNatural (Index ss)
  , Eq se
  , TakeWhile c s m n ss se w
  )
  => se
  -> Iso c m n s Natural
countElement x = toEnum . length <$> takeWhile (== x) `upon` flip replicate x . fromNatural

-- | Counts 1 or more elements
countElementSome :: forall c s m n ss se w.
  ( FromNatural (Index ss)
  , SubStateContext c s
  , IsSequence ss
  , MonadState s m
  , MonadFail m
  , MonadWriter w n
  , ConvertSequence c ss w n
  , MonadFail n
  , Eq se
  , SelectableStateT c
  , se ~ SubElement s
  , ss ~ SubState s
  )
  => SubElement s
  -> Biparser c s m n Natural Natural
countElementSome x = do
  c <- countElement x
  unless (c > 0) $ fail "countElementSome expected at least one element match but found none"
  return c

class FromNatural a where fromNatural :: Natural -> a
instance FromNatural Int where fromNatural = fromEnum

not :: forall c s m n u.
  ( Functor m
  , Functor n
  )
  => Biparser c s m n u Bool
  -> Biparser c s m n u Bool
not = fmap Data.Bool.not

-- | If success, returns True. If fails, returns False
failBool :: (Applicative m, Alt m) => m a -> m Bool
failBool x = x $> True <!> pure False

-- | Causes backward to write nothing.
memptyWrite :: forall c s m n u v w.
  MonadWriter w n
  => Biparser c s m n u v
  -> Biparser c s m n u v
memptyWrite = flip mapWrite (const mempty)
