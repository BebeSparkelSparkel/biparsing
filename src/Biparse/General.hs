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
  , dropWhile
  , skipUntil
  , untilJust
  , Pad
  , pad
  , padCount
  , BreakWhen
  , breakWhen
  , optionMaybe
  , optional
  , stripPrefix
  , countElement
  , countElementSome
  , FromNatural(..)
  , not
  , memptyWrite
  , rest
  , failForward
  , failBackward
  ) where

import Control.Profunctor.FwdBwd (endoSecond)
import Biparse.Biparser (Biparser, Iso, Unit, unit, one, try, SubState, SubElement, ElementContext, SubStateContext, split, Const, mapWrite, Unit, ignoreForward, upon, uponM, comap, comapM, count, resetState)
import Data.Bool qualified
import Data.EqElement qualified
import Control.Profunctor.FwdBwd (firstM)

identity :: forall c s m n ss.
  ( MonadState s m
  , MonadWriter ss n
  , SubStateContext c s
  , ss ~ SubState c s
  )
  => Iso c m n s ss
identity = split do
  x <- get
  put mempty
  return x

-- * Take for single elements

type Take c s m n ss se =
  ( Show (SubElement c s)
  , Eq (SubElement c s)
  , IsSequence (SubState c s)
  , MonadState s m
  , MonadFail m
  , MonadPlus m
  , MonadWriter (SubState c s) n
  , MonadFail n
  , ElementContext c s
  -- assignments
  , ss ~ SubState c s
  , se ~ SubElement c s
  )

-- | Assumes but disregards the writer context
-- Should not be used with Alternative
take :: forall c s m n u ss se. Take c s m n ss se => se -> Const c s m n u
take takeWrite = unit $ takeUnit takeWrite

-- | Discards the match
-- Should not be used with Alternative
takeUnit :: forall c s m n ss se. Take c s m n ss se => se -> Unit c s m n
takeUnit takeWrite = void $ takeUni takeWrite `upon` const takeWrite

-- | Returns the match
takeUni :: forall c s m n ss se.
  Take c s m n ss se
  => se
  -> Iso c m n s se
takeUni takeWriteMatchReturn = try do
  y <- one
  unless (takeWriteMatchReturn == y) $ expectedFail takeWriteMatchReturn y
  return y

takeDi :: forall c s m n u ss se.
  ( Take c s m n ss se
  , Eq u
  , Alternative n
  )
  => se
  -> u
  -> Iso c m n s u
takeDi takeWrite matchReturn = takeTri takeWrite matchReturn matchReturn

-- | Allows 'SubElement c s'`, 'u', and 'v' to be fixed which works well with Alternative.
takeTri :: forall c s m n u v ss se.
  ( Take c s m n ss se
  , Eq u
  , Alternative n
  )
  => se
  -> u
  -> v
  -> Biparser c s m n u v
takeTri takeWrite toMatch toReturn = try do
  x <- one `uponM` ($> takeWrite) . guard . (== toMatch)
  unless (takeWrite == x) $ expectedFail takeWrite x
  return toReturn

expectedFail :: (MonadFail m, Show a, Show b) => a -> b -> m ()
expectedFail x y = fail $ "Expected a " <> show x <> " but received a " <> show y

takeNot :: forall c s m n ss se.
  Take c s m n ss se
  => se
  -> Iso c m n s se
takeNot x = try do
  y <- one
  when (x == y) $ fail $ "Should not have found an " <> show y
  return y

-- * Take for prefixes

type Take' c s m n ss se =
  -- m
  ( MonadPlus m
  , MonadState s m
  , MonadFail m
  -- n
  , Monad n
  , MonadWriter ss n
  -- substate
  , Show ss
  , EqElement ss
  -- context
  , SubStateContext c s
  -- assignments
  , ss ~ SubState c s
  , se ~ SubElement c s
  )

takeDi' :: forall c s m n u ss se.
  ( Take' c s m n ss se
  , Eq u
  , Alternative n
  )
  => ss
  -> u
  -> Iso c m n s u
takeDi' takeWrite matchReturn = takeTri' takeWrite matchReturn matchReturn

-- | Allows 'SubState c s'`, 'u', and 'v' to be fixed which works well with Alternative.
takeTri' :: forall c s m n u v ss se.
  ( Eq u
  , Alternative n
  , Take' c s m n ss se
  )
  => SubState c s
  -> u
  -> v
  -> Biparser c s m n u v
takeTri' takeWrite toMatch toReturn = try do
  stripPrefix takeWrite `uponM` guard . (== toMatch)
  return toReturn

-- * Take while predicate

type TakeWhile c s m n se =
  ( SubStateContext c s
  , IsSequence (SubState c s)
  , MonadState s m
  , MonadWriter (SubState c s) n
  , se ~ SubElement c s
  )

takeWhile :: forall c s m n se.
  TakeWhile c s m n se
  => (se -> Bool)
  -> Iso c m n s (SubState c s)
takeWhile = split . state . span

-- | Drop forward elements while predicate is true.
dropWhile :: forall c s m n u se.
  TakeWhile c s m n se
  => (se -> Bool)
  -> Const c s m n u
dropWhile = void . comap (const mempty) . takeWhile

-- | Run until returns True
skipUntil :: forall c s m n u.
  ( MonadState s m
  , Monad n
  , Monoid (SubState c s)
  )
  => Biparser c s m n u Bool
  -> Const c s m n u
skipUntil x = bool (void $ skipUntil x) (pure ()) =<< resetState id x

-- | Run until Just
untilJust :: forall c s m n u a ss.
  ( Monad m
  , Monad n
  , Monoid ss
  , ss ~ SubState c s
  )
  => Biparser c s m n u (Maybe a)
  -> Biparser c s m n u a
untilJust x = maybe (untilJust x) pure =<< x

-- * Pad

type Pad c s m n u v ss i se =
  -- m
  ( MonadState s m
  -- n
  , MonadWriter ss n
  -- substate
  , IsSequence ss
  , SubStateContext c s
  , Eq se
  -- i
  , Ord i
  , Num i
  , ConvertIntegral Natural i
  -- Assignments
  , ss ~ SubState c s
  , se ~ SubElement c s
  , i ~ Index ss
  )

-- | Consumes the pad 'c' charcaters forward. Prepends the pad 'c' caracters backwards to ensure there are 'n' charcaters written.
pad :: forall c s m n u v ss i se.
  Pad c s m n u v ss i se
  => Natural
  -> se
  -> Biparser c s m n u v
  -> Biparser c s m n u v
pad (convertIntegral -> n) c x = do
  dropWhile (== c)
  mapWrite x \y ->
    let l = lengthIndex y
    in if l >= n
      then y
      else replicate (n - l) c <> y

-- | Gives the pad count found for forward (number of c + number consumed by x). Just returns n backwards.
padCount :: forall c s m n u v ss i se.
  Pad c s m n u v ss i se
  => Natural
  -> se
  -> Biparser c s m n u v
  -> Biparser c s m n u (Natural, v)
padCount n c x = endoSecond (first $ const n) $ count $ pad n c x

-- * Break

type BreakWhen c s m n ss =
  ( IsSequence ss
  , MonadState s m
  , MonadFail m
  , MonadPlus m
  , MonadWriter ss n
  , Alternative n
  , ElementContext c s
  , ss ~ SubState c s
  )

-- | Breaks off the substate head when 'x' succeeds. Writes x after given 'ss'.
-- DEV NOTE: Seems like there could be a more simplistic solution.
breakWhen :: forall c s m n ss.
  BreakWhen c s m n ss
  => Unit c s m n
  -> Iso c m n s ss
breakWhen x
  = bw <* ignoreForward () (unit x)
  where
  bw
    =   mempty <$ failBackward (try $ unit x)
    <|> do
          y <- one `uponM` headAlt
          cons y <$> bw `uponM` tailAlt
    <|> pure mempty

---- | Like 'breakWhen' but fails if 'x' does not succeed
--breakWhen' :: forall c s m n ss.
--  BreakWhen c s m n ss
--  => Unit c s m n
--  -> Iso c m n s ss
--breakWhen' x
--  = bw
--  <|> ignoreForward  (write *> unit x)
--  where
--  bw
--    =   mempty <$ (failBackward $ try $ unit x)
--    <|> do
--          y <- one `uponM` headAlt
--          cons y <$> breakWhen' x `uponM` tailAlt

-- | Consumes rest/all of substate and writes given
rest :: forall c s m n ss.
  ( MonadState s m
  , MonadWriter ss n
  , SubStateContext c s
  , ss ~ SubState c s
  )
  => Iso c m n s ss
rest = split $ get <* put mempty

-- * failure

failForward :: forall c s m n u v.
  Alternative m
  => Biparser c s m n u v
  -> Biparser c s m n u v
failForward = firstM $ const empty

failBackward :: forall c s m n u v.
  ( Monad n
  , Alternative n
  )
  => Biparser c s m n u v
  -> Biparser c s m n u v
failBackward = comapM $ const empty

-- * Optional parsing

optionMaybe :: forall c s m n u v.
  ( Monoid (SubState c s)
  , MonadPlus m
  , MonadState s m
  , Alternative n
  )
  => Biparser c s m n u v
  -> Biparser c s m n u (Maybe v)
optionMaybe x = Just <$> try x <|> pure Nothing

-- | Allows a parser to fail and return Maybe instead. Allows writer to optionally run or not.
optional :: forall c s m n u v.
  ( MonadPlus m
  , MonadState s m
  , Monad n
  , Alternative n
  , Monoid (SubState c s)
  )
  => Biparser c s m n u v
  -> Biparser c s m n (Maybe u) (Maybe v)
optional x = Just <$> try x `uponM` maybe empty pure <|> pure Nothing

-- * Stripping

stripPrefix :: forall c s m n ss u.
  ( ss ~ SubState c s
  , EqElement ss
  , SubStateContext c s
  , Show ss
  , MonadState s m
  , MonadFail m
  , MonadWriter ss n
  )
  => ss
  -> Const c s m n u
stripPrefix pre = unit $ void s `upon` const pre
  where
  s :: Iso c m n s ss
  s = split $ StateT
    $ maybe
      (fail $ "Could not strip prefix: " <> show pre)
      (pure . (pre,))
    . Data.EqElement.stripPrefix pre

-- * Counting

-- | Counts 0 or more elements
countElement :: forall c s m n se.
  ( FromNatural (Index (SubState c s))
  , Eq se
  , TakeWhile c s m n se
  )
  => se
  -> Iso c m n s Natural
countElement x = toEnum . length <$> takeWhile (== x) `upon` flip replicate x . fromNatural

-- | Counts 1 or more elements
countElementSome :: forall c s m n ss se.
  ( FromNatural (Index ss)
  , SubStateContext c s
  , IsSequence ss
  , MonadState s m
  , MonadPlus m
  , MonadWriter ss n
  , Alternative n
  , Eq se
  , se ~ SubElement c s
  , ss ~ SubState c s
  )
  => SubElement c s
  -> Biparser c s m n Natural Natural
countElementSome x = do
  c <- countElement x
  unless (c > 0) empty
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

-- | Causes backward to write nothing.
memptyWrite :: forall c s m n u v ss.
  MonadWriter ss n
  => Biparser c s m n u v
  -> Biparser c s m n u v
memptyWrite = flip mapWrite (const mempty)

