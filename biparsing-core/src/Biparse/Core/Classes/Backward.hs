{-# LANGUAGE UndecidableInstances #-}
module Biparse.Core.Classes.Backward (
UnfoldlExactN(..),
) where

import Biparse.Core.Aliases (CPSWriterT, LazyWriterT, StrictWriterT, LazyStateT, StrictStateT, CPSRWST, LazyRWST, StrictRWST)
import Control.Monad.Writer (tell)
import Data.MonoTraversable (MonoPointed(opoint), Element)

class UnfoldlExactN m a | m -> a where
  unfoldlExactN :: Int -> (b -> (b, a)) -> b -> m b

---- * Only One Direction
--
---- | Only run a backwards operation
--onlyBackwards :: Applicative p => (u -> n ()) -> Const p u 
--onlyBackwards = undefined
----onlyBackwards = Biparser $ pure ()

---- * Constrained Subtypes
---- More constrained subtypes of Biparser

---- | Throws away the forward computation and returns 'x'. Only the backwards computation runs.
--ignoreBackward :: forall p u u' v.
--  ()
--  => v
--  -> Biparser p u  v
--  -> Biparser p u' v
--ignoreBackward = undefined
----ignoreBackward x y = setBackward y $ const $ pure x
--
--ignoreBackwardIso :: forall p a.
--  ()
--  => Iso c t p s a
--  -> Iso c t p s a
--ignoreBackwardIso = undefined
----ignoreBackwardIso = flip setBackward pure

--failBackward :: forall c s m n u v.
--  MonadFail n
--  => Biparser m u v
--  -> Biparser m u v
--failBackward = undefined
----failBackward = comapM $ const $ fail "pureposely failed in the backwards direction"

