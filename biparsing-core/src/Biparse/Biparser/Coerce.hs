{-# OPTIONS_GHC -Wno-redundant-constraints #-}
module Biparse.Biparser.Coerce (
coerceIso,
) where

import Biparse.Biparser.Internal (Iso)
import Data.Coerce (Coercible)
import Unsafe.Coerce (unsafeCoerce)

coerceIso :: (Coercible b b', Coercible b' b) => Iso c m n a b -> Iso c m n a b'
coerceIso = unsafeCoerce

