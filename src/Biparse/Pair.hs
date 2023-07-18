module Biparse.Pair
  ( tupleFst
  ) where

import Biparse.Biparser (Iso, upon, SubState)

-- | Used to match and fill the first element in the tuple
tupleFst :: forall c m em n s a b.
  ( MonadPlus m
  , Eq a
  , Monad n
  , Alternative n
  , Monoid (SubState c s)
  )
  => a
  -> Iso c m em n s (a,b)
  -> Iso c m em n s b
tupleFst x i = do
  (y,z) <- i `upon` (x,)
  unless (x == y) empty
  return z

