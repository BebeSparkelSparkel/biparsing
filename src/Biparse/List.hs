{-# LANGUAGE PolyKinds #-}

module Biparse.List
  ( replicateBiparserT
  , takeElementsWhile
  , Many
  , many
  , manyId
  , manyIso
  , some
  , someIso
  , all
  , splitElem
  , splitOn
  , whileM
  , whileM'
  --, whileId
  --, untilM
  --, untilM'
  --, untilId
  , headAlt
  , tailAlt
  ) where

import Biparse.Biparser (Biparser, uponM, Iso, SubElement, SubState, emptyForward, one, try, upon, mono, ElementContext, FixFail, fix, peek, Unit, UpdateStateWithSubState, isNull, breakWhen', GetSubState)
import Biparse.General (take, takeNot, Take, memptyWrite, BreakWhen, rest)
import Data.List.NonEmpty (NonEmpty, nonEmpty)

replicateBiparserT :: forall c s m n u v.
  ( Monoid (SubState c s)
  , Monad m
  , MonadFail n
  )
  => Int
  -> Biparser c s m n u v
  -> Biparser c s m n [u] [v]
replicateBiparserT = \case
  0 -> const $ return $ mempty
  n -> \x -> do
    v <- x `uponM` (emptyFail n . headMay)
    vs <- replicateBiparserT (n - 1) x `uponM` (emptyFail n . tailMay)
    return $ v : vs
  where
  emptyFail :: Int -> Maybe a -> n a
  emptyFail n = maybe
    (fail $ "Expected " <> show n <> " more elements but there are none left.")
    return

-- | Takes
takeElementsWhile :: forall c s m n ss.
   ( IsSequence ss
   , MonadFail m
   , MonadPlus m
   , MonadState s m
   , MonadWriter ss n
   , Alternative n
   , ElementContext c s
   , ss ~ SubState c s
   )
  => (SubElement c s -> Bool)
  -> Iso c m n s [SubElement c s]
takeElementsWhile f =
  try do
    x <- one `uponM` headAlt
    unless (f x) emptyForward
    (x :) <$> takeElementsWhile f `uponM` tailAlt
  <|> return mempty

type Many c s m n =
  ( Monoid (SubState c s)
  , MonadPlus m
  , Monad n
  , Alternative n
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
    (y :) <$> many x `uponM` tailAlt
  <|> pure mempty

manyId :: forall c s m n u v.
  ( FixFail m
  , FixFail n
  , Monoid (SubState c s)
  , MonadPlus m
  , Monad n
  , Alternative n
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
all :: forall c s m n u v ss.
  ( Monoid ss
  , MonoFoldable ss
  , GetSubState c s
  , MonadState s m
  , Monad n
  , Alternative n
  , ss ~ SubState c s
  )
  => Biparser c s m n u v
  -> Biparser c s m n [u] [v]
all x = ifM isNull (pure mempty) do
  y <- x `uponM` headAlt
  (y :) <$> all x `uponM` tailAlt

-- | Splits the substate on given element
splitElem :: forall c s m n ss.
  ( Take c s m n
  , Many c s m n
  , ss ~ SubState c s
  )
  => SubElement c s
  -> Iso c m n s [ss]
splitElem x = correctEmpty splitter
  where
  splitter :: Iso c m n s [ss]
  splitter = do
    y <- fromList <$> manyIso (takeNot x) `uponM` fmap toList . headAlt
    take x *> ((y :) <$> splitter `uponM` tailAlt) <|> pure (singleton y)

  correctEmpty :: Iso c m n s [ss] -> Iso c m n s [ss]
  correctEmpty = mono \case
    [y] | null y -> mempty
    [] -> [mempty]
    y -> y

splitOn :: forall c s m n ss.
  ( BreakWhen c s m n ss
  , UpdateStateWithSubState c s
  )
  => Unit c s m n
  -> Iso c m n s [ss]
splitOn x
  =   ifM isNull (pure mempty)
  $   do
        hs <- so `uponM` initAlt 
        l <- rest `uponM` lastAlt
        return $ hs `snoc` l
  <|> singleton <$> rest `upon` const mempty
  where
  so = ifM isNull
    (pure mempty)
    (breakWhen' x `uponM` headAlt ^:^ (so <|> pure mempty) `uponM` tailAlt)

whileM :: forall c s m n u v ss.
  ( MonadPlus m
  , MonadState s m
  , MonadWriter ss n
  , Alternative n
  , ss ~ SubState c s
  )
  => Biparser c s m n u Bool
  -> Biparser c s m n u v
  -> Biparser c s m n [u] [v]
whileM p = whileM' (peek $ memptyWrite p)

whileM' :: forall c s m n u v.
  ( MonadPlus m
  , Monad n
  , Alternative n
  , Monoid (SubState c s)
  )
  => Biparser c s m n u Bool
  -> Biparser c s m n u v
  -> Biparser c s m n [u] [v]
whileM' p x = ifM (p `uponM` headAlt <|> pure False)
  (x `uponM` headAlt ^:^ whileM' p x `uponM` tailAlt)
  (pure mempty)

-- | Should be able to use ghosts of departed prrofs to get rid of partial head tail
--whileId :: forall c s u v.
--  ( Monoid (SubState c s)
--  )
--  => Biparser c s Identity Identity u Bool
--  -> Biparser c s Identity Identity u v
--  -> Biparser c s Identity Identity [u] [v]
--whileId p x = ifM (p `uponMay` False $ headMay)
--  ( x `upon` unsafeHead ^:^ whileId p x `upon` unsafeTail)
--  (pure mempty)

--untilM :: forall c s m n u v.
--  ( MonadPlus m
--  , Monad n
--  , Alternative n
--  , Monoid (SubState c s)
--  )
--  => Biparser c s m n u Bool
--  -> Biparser c s m n u v
--  -> Biparser c s m n [u] [v]
--untilM = undefined
--
--untilM' :: forall c s m n u v.
--  ( MonadPlus m
--  , Monad n
--  , Alternative n
--  , Monoid (SubState c s)
--  )
--  => Biparser c s m n u Bool
--  -> Biparser c s m n u v
--  -> Biparser c s m n [u] [v]
--untilM' = undefined

-- | Should be able to use ghosts of departed prrofs to get rid of undefined
--untilId :: forall c s u v.
--  ( Monoid (SubState c s)
--  )
--  => Biparser c s Identity Identity u Bool
--  -> Biparser c s Identity Identity u v
--  -> Biparser c s Identity Identity [u] [v]
--untilId = whileId . mapFW Data.Bool.not

headAlt :: forall a n. (Alternative n, MonoFoldable a) => a -> n (Element a)
headAlt = maybe empty pure . headMay

lastAlt :: forall a n. (Alternative n, MonoFoldable a) => a -> n (Element a)
lastAlt = maybe empty pure . lastMay

tailAlt :: forall a n. (Alternative n, IsSequence a) => a -> n a
tailAlt = maybe empty pure . tailMay

initAlt :: forall a n. (Alternative n, IsSequence a) => a -> n a
initAlt = maybe empty pure . initMay

