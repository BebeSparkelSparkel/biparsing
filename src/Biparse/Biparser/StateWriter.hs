module Biparse.Biparser.StateWriter
  ( translate
  ) where

import Biparse.Biparser (Biparser(Biparser))


-- | Discards unused s' state to avoid commingling m and n monads.
translate :: forall c' c s s' m n ss ss' u v.
   ( MonadFail m
   , Monad n
   )
  => Biparser c  s  (StateT s  m) (WriterT ss  n) ss' s'
  -> Biparser c' s' (StateT s' m) (WriterT ss' n) u   v
  -> Biparser c  s  (StateT s  m) (WriterT ss  n) u   v
translate (Biparser fw bw) (Biparser fw' bw') = Biparser
  (StateT \s -> do
    (s',s'') <- runStateT fw s
    (x, _) <- runStateT fw' s'
    pure (x,s'')
  )
  \u -> WriterT do
    (x,w)  <- runWriterT (bw' u)
    (_,w') <- runWriterT (bw  w)
    pure (x,w')

