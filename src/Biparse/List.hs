{-# LANGUAGE PolyKinds #-}

module Biparse.List
  ( replicateBiparserT
  , takeElementsWhile
  , many
  , manyId
  , manyIso
  , some
  , someIso
  , splitElem
  , whileM
  , whileM'
  , whileId
  --, untilM
  --, untilM'
  , untilId
  , headAlt
  , tailAlt
  ) where

import Control.Monad.Extra (ifM)
import Data.Bool (Bool(True,False))
import Data.Bool qualified as Data.Bool
import Biparse.BiparserT (BiparserT, uponM, Iso, SubElement, SubState, emptyForward, one, try, upon, mono, ElementContext, FixFail, fix, mapMs, optionalBack, mapBack, peek, uponMay, mapFW)
import Data.Functor.Identity (Identity(runIdentity))
import Control.Applicative (Applicative(pure), Alternative((<|>),empty), (*>), liftA2)
import Biparse.General (take, takeNot, Take, memptyBack, not)
import Data.Functor ((<$>), fmap)
import Control.Monad (Monad(return), MonadFail(fail), MonadPlus, unless)
import Data.Function ((.), const, ($))
import Data.Int (Int)
import Data.MonoTraversable (headMay, unsafeHead)
import Data.MonoTraversable.Unprefixed (toList, null)
import Data.Monoid (Monoid(mempty), (<>))
import Data.Sequences (IsSequence, tailMay, fromList, singleton, unsafeTail)
import GHC.Num ((-))
import Text.Show (Show(show))
import Data.Maybe (Maybe, maybe)
import Data.List.NonEmpty (NonEmpty, nonEmpty)

import Prelude (undefined)
import Debug.Trace

replicateBiparserT :: forall c s m n u v.
  ( Monoid (SubState c s)
  , Monad m
  , MonadFail n
  )
  => Int
  -> BiparserT c s m n u v
  -> BiparserT c s m n [u] [v]
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
takeElementsWhile :: forall c s m n.
   ( IsSequence (SubState c s)
   , MonadFail m
   , MonadPlus m
   , Monad n
   , Alternative n
   , ElementContext c s
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
  => BiparserT c s m n u v
  -> BiparserT c s m n [u] [v]

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
  => BiparserT c s m n u v
  -> BiparserT c s Identity Identity [u] [v]
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
  => BiparserT c s m n u v
  -> BiparserT c s m n (NonEmpty u) (NonEmpty v)
some x = do
  ys <- many x `upon` toList
  maybe (fail "Expected to parse at least one but parsed none.") pure $ nonEmpty ys

-- | Iso version of 'some'
someIso :: forall c s m n a.
  Some c s m n
  => Iso c m n s a
  -> Iso c m n s (NonEmpty a)
someIso = some

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

whileM :: forall c s m n u v.
  ( MonadPlus m
  , Monad n
  , Alternative n
  , Monoid (SubState c s)
  )
  => BiparserT c s m n u Bool
  -> BiparserT c s m n u v
  -> BiparserT c s m n [u] [v]
whileM p = whileM' (peek $ memptyBack p)

whileM' :: forall c s m n u v.
  ( MonadPlus m
  , Monad n
  , Alternative n
  , Monoid (SubState c s)
  )
  => BiparserT c s m n u Bool
  -> BiparserT c s m n u v
  -> BiparserT c s m n [u] [v]
whileM' p x = ifM (p `uponM` headAlt <|> pure False)
  (x `uponM` headAlt ^:^ whileM' p x `uponM` tailAlt)
  (pure mempty)

-- | Should be able to use ghosts of departed prrofs to get rid of partial head tail
whileId :: forall c s u v.
  ( Monoid (SubState c s)
  )
  => BiparserT c s Identity Identity u Bool
  -> BiparserT c s Identity Identity u v
  -> BiparserT c s Identity Identity [u] [v]
whileId p x = ifM (p `uponMay` False $ headMay)
  ( x `upon` unsafeHead ^:^ whileId p x `upon` unsafeTail)
  (pure mempty)

infixr 5 ^:^
(^:^) :: Applicative f => f a -> f [a] -> f [a]
(^:^) = liftA2 (:)

--untilM :: forall c s m n u v.
--  ( MonadPlus m
--  , Monad n
--  , Alternative n
--  , Monoid (SubState c s)
--  )
--  => BiparserT c s m n u Bool
--  -> BiparserT c s m n u v
--  -> BiparserT c s m n [u] [v]
--untilM = undefined
--
--untilM' :: forall c s m n u v.
--  ( MonadPlus m
--  , Monad n
--  , Alternative n
--  , Monoid (SubState c s)
--  )
--  => BiparserT c s m n u Bool
--  -> BiparserT c s m n u v
--  -> BiparserT c s m n [u] [v]
--untilM' = undefined

-- | Should be able to use ghosts of departed prrofs to get rid of undefined
untilId :: forall c s u v.
  ( Monoid (SubState c s)
  )
  => BiparserT c s Identity Identity u Bool
  -> BiparserT c s Identity Identity u v
  -> BiparserT c s Identity Identity [u] [v]
untilId = whileId . mapFW Data.Bool.not

headAlt :: forall n a. Alternative n => [a] -> n a
headAlt = maybe empty pure . headMay

tailAlt :: forall n a. Alternative n => [a] -> n [a]
tailAlt = maybe empty pure . tailMay

