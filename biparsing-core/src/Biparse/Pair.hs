module Biparse.Pair
  ( tupleFst
  ) where

import Biparse.Biparser (Iso, upon)

-- | Used to match and fill the first element in the tuple
tupleFst :: forall c m n s a b.
  ( MonadFail m
  , Eq a
  , MonadFail n
  )
  => a
  -> Iso c m n s (a,b)
  -> Iso c m n s b
tupleFst x i = do
  (y,z) <- i `upon` (x,)
  unless (x == y) (fail "first tuple value does not match given")
  return z

