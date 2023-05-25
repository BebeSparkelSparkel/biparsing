module Biparse.List
  ( replicateBiparserT
  , splitElem
  ) where

import Biparse.BiparserT (BiparserT, uponM, Iso, iso)
import Control.Applicative (Alternative(some))
import Control.Monad (Monad(return), MonadFail(fail), MonadPlus)
import Data.Eq (Eq)
import Data.Function ((.), const, ($))
import Data.Int (Int)
import Data.Maybe (Maybe(Just,Nothing), maybe)
import Data.MonoTraversable (Element, headMay, ointercalate)
import Data.Monoid (Monoid(mempty), (<>))
import Data.Sequences (IsSequence, tailMay, singleton)
import Data.Sequences qualified as MT
import Data.Traversable (traverse)
import GHC.Num ((-))
import Text.Show (Show(show))

replicateBiparserT :: forall sw m n u v. (Monoid sw, Monad m, MonadFail n) => Int -> BiparserT sw m n u v -> BiparserT sw m n [u] [v]
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

splitElem :: forall sw m n.
  ( Monad m
  , Monad n
  , Eq (Element sw)
  , IsSequence sw
  )
  => Element sw
  -> Iso m n sw [sw]
splitElem x = iso (MT.splitElem x) (ointercalate (singleton x))


-- | Applies given biparser zero or more times.
-- DEV QUESTION: Is it possible to use Alternative(many)?
--many :: forall sw m n u v.
--  ( Monoid sw
--  , MonadPlus m
--  , Monad n
--  , Alternative n
--  )
--  => BiparserT sw m n u v
--  -> BiparserT sw m n [u] [v]
--many x = BiparserT
--  (do
--    y <- forward $ optionMaybe x
--    case y of
--      Just z -> (z :) <$> forward (many x)
--      Nothing -> pure mempty
--  )
--  (\case
--    u:us -> liftA2 (:) (backward x u) (backward (many x) us)
--    [] -> pure mempty
--  )
--many :: (Monoid sw, MonadPlus m, Alternative n) => BiparserT sw m n u v -> BiparserT sw m n [u] [v]
--many (BiparserT fw bw) = BiparserT (some fw) (traverse bw)
