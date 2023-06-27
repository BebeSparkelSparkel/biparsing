module Biparse.Text.Numeric
  ( intBaseTen
  ) where

import Biparse.Biparser (Iso, split, upon, SubState, SubStateContext)
import Safe (readMay)
import Data.Char (isDigit)
import Biparse.Text (CharElement)

intBaseTen :: forall c s m n a text.
  ( CharElement c s
  , Show a
  , IsSequence text
  , MonadState s m
  , MonadFail m
  , MonadWriter text n
  , MonadFail n
  , Read a
  , SubStateContext c s
  , text ~ SubState c s
  )
  => Iso c m n s a
intBaseTen = do
  digits <- split (state $ span isDigit) `upon` fromList . show
  maybe (fail "Could not parse integer base 10.") pure $ readMay $ toList digits
  
