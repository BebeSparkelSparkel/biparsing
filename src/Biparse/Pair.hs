module Biparse.Pair
  ( tupleFst
  ) where

import Biparse.Biparser (Iso, upon, SubState)

-- | Used to match and fill the first element in the tuple
tupleFst :: forall c m n s a b.
  ( MonadPlus m
  , Eq a
  , Monad n
  , Alternative n
  , Monoid (SubState s)
  )
  => a
  -> Iso c m n s (a,b)
  -> Iso c m n s b
tupleFst x i = do
  (y,z) <- i `upon` (x,)
  unless (x == y) empty
  return z

