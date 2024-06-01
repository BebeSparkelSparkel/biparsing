{-# LANGUAGE NumDecimals #-}
module SpecHook where

import System.Timeout (timeout)
import Control.Monad ((<=<))

hook :: Spec -> Spec
hook = around_ $ maybe (fail "exceeded timeout") pure <=< timeout 10000

