module Biparse.Biparser.StateWriter
  ( translate
  , Biparser
  , Iso
  , Unit
  , Const
  , ConstU
  ) where

import Biparse.Biparser (SubState)
import Biparse.Biparser qualified as B

type Biparser c s m n u v = B.Biparser c s (StateT s m) (WriterT (SubState c s) n) u v 
type Iso c m n s v = Biparser c s m n v v
type Unit c s m n = Biparser c s m n () ()
type Const c s m n u = Biparser c s m n u ()
type ConstU c s m n u v = Biparser c s m n u v

-- | Discards unused s' state to avoid commingling m and n monads.
translate :: forall c' c s s' m n ss' u v.
   ( MonadFail m
   , Monad n
   , ss' ~ SubState c' s'
   )
  => Biparser c  s  m n ss' s'
  -> Biparser c' s' m n u   v
  -> Biparser c  s  m n u   v
translate (B.Biparser fw bw) (B.Biparser fw' bw') = B.Biparser
  (StateT \s -> do
    (s',s'') <- runStateT fw s
    (x, _) <- runStateT fw' s'
    pure (x,s'')
  )
  \u -> WriterT do
    (x,w)  <- runWriterT (bw' u)
    (_,w') <- runWriterT (bw  w)
    pure (x,w')

