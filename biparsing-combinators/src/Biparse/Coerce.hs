{-# OPTIONS_GHC -Wno-redundant-constraints #-}
module Biparse.Coerce (
coerceIso,
) where

import Data.Coerce (Coercible)
import Unsafe.Coerce (unsafeCoerce)

coerceIso :: (Coercible b b', Coercible b' b) => Iso p b -> Iso p b'
coerceIso = unsafeCoerce

