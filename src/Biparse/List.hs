module Biparse.List
  ( replicateBiparserT
  , takeWhile
  , splitElem
  , many
  ) where

import Data.Bool (Bool)
import Biparse.BiparserT (BiparserT, uponM, Iso, SubElement, SubState, StateContext, emptyForward, one, try, upon, comapM)
import Control.Applicative (Alternative((<|>),empty), pure, (<*))
import Biparse.General (take, takeNot, Take)
import Data.Functor ((<$>))
import Control.Monad (Monad(return), MonadFail(fail), MonadPlus, unless)
import Data.Function ((.), const, ($))
import Data.Int (Int)
import Data.MonoTraversable (headMay, otoList, onull, lastMay)
import Data.Monoid (Monoid(mempty), (<>))
import Data.Sequences (IsSequence, tailMay, fromList, initMay, snoc)
import GHC.Num ((-))
import Text.Show (Show(show))
import Data.Maybe (Maybe, maybe)


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

takeWhile :: forall c s m n.
   ( IsSequence (SubState c s)
   , MonadFail m
   , MonadPlus m
   , Monad n
   , Alternative n
   , StateContext c s
   )
  => (SubElement c s -> Bool)
  -> Iso c m n s [SubElement c s]
takeWhile f =
  try do
    x <- one `uponM` headAlt
    unless (f x) emptyForward
    (x :) <$> takeWhile f `uponM` tailAlt
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

-- | Iso version of 'many'
many' :: forall c s m n a.
  Many c s m n
  => Iso c m n s a
  -> Iso c m n s [a]
many' = many

-- | Splits the substate on given element
-- DEV NOTE: I think this is a terrible impelmentation. It should not have to use init and last.
splitElem :: forall c s m n.
  ( Take c s m n
  , Many c s m n
  )
  => SubElement c s
  -> Iso c m n s [SubState c s]
splitElem x =
  do
    xs <- comapM initAlt $ many' $ cs <* take x
    l <- comapM lastAlt $ cs
    return case xs `snoc` l of
      [z] | onull z -> mempty
      z -> z
  <|> pure mempty
  where
  cs = fromList <$> many' (takeNot x) `upon` otoList

headAlt :: Alternative n => [a] -> n a
headAlt = maybe empty pure . headMay

tailAlt :: Alternative n => [a] -> n [a]
tailAlt = maybe empty pure . tailMay

initAlt :: Alternative n => [a] -> n [a]
initAlt = maybe empty pure . initMay

lastAlt :: Alternative n => [a] -> n a
lastAlt = maybe empty pure . lastMay

