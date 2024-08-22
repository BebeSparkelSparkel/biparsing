{-# LANGUAGE UndecidableInstances #-}
module Biparse.Core.Classes.Backward (
OneBwd(..),
PutBwd(..),
UnfoldlExactN(..),
) where

import Data.MonoTraversable (MonoPointed(opoint), Element)
import Biparse.Core.Aliases (LazyWriterT, LazyRWST)
import Control.Monad.Writer (tell)

class OneBwd a m | m -> a where oneBwd :: a -> m ()
instance (OneBwd a m, Monad m) => OneBwd a (ReaderT r m) where oneBwd = lift . oneBwd
instance (Monad m, MonoPointed w, Monoid w, Element w ~ a) => OneBwd a (LazyWriterT w m) where oneBwd = tell . opoint
instance (Monad m, MonoPointed w, Monoid w, Element w ~ a) => OneBwd a (LazyRWST r w s m) where oneBwd = tell . opoint

class PutBwd a m | m -> a where putBwd :: a -> m ()
instance (Monad m, Monoid w) => PutBwd w (LazyWriterT w m)  where putBwd = tell
instance (Monad m, Monoid w) => PutBwd w (LazyRWST r w s m) where putBwd = tell

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

