module Biparse.Zoom (
zoom,
--zoomWrite,
--zoomOne,
) where

import GHC.Err (undefined)

zoom :: forall m m' m'' ss u v.
  ()
  => Iso m ss
  -> Biparser m' u v
  -> Biparser m'' u v
zoom = undefined

---- | Strict on the Iso which makes 'Biparser' slow to run if not all 'ss\'' is requred and for error to be thrown.
--zoom :: forall is c' mProgenitor w m' c ss' s s' m n r ws u v.
--  ( Monad m
--  , Monad n
--  , ReplaceSubState s ss' s'
--  , ChangeMonad is m' m ()
--  --, ChangeFunction is m' m ~ ()
--  --
--  , BackwardC c  n r w   ws
--  , BackwardC c' n r ss' ws
--  -- Backward
--  , Default (BackwardArgC c)
--  , Default (BackwardArgC c')
--  -- assignments
--  , m  ~ MonadProgenitor mProgenitor s
--  , m' ~ MonadProgenitor mProgenitor s'
--  )
--  => Iso c m n r w ws s ss'
--  -> Biparser c' s' (MonadProgenitor mProgenitor s') n r ss' ws u v
--  -> Biparser c  s  (MonadProgenitor mProgenitor s)  n r w  ws u v
--zoom (B.Biparser fw bw) (B.Biparser fw' bw') = B.Biparser
--  (StateErrorT \s -> do
--    (ss,s') <- runStateErrorT fw s
--    (x,_) <- changeMonad' @is () $ runStateErrorT fw' $ replaceSubState s ss
--    pure (x,s')
--  )
--  \u -> backwardT @c \r s -> do
--    (x,s',w)   <- runBackwardT @c' (bw' u)  def r s
--    (_,s'',w') <- runBackwardT @c  (bw $ w) def r s'
--    pure (x,s'',w')
--
---- | Strict on the Iso which makes 'Biparser' slow to run if not all 'ss\'' is requred and for error to be thrown.
--zoomWrite :: forall is c' mProgenitor w m' c ss' s s' m n r ws u v.
--  -- m
--  ( Monad m
--  , ChangeMonad is m' m ()
--  --, ChangeFunction is m' m ~ ()
--  -- n
--  , Monad n
--  , BackwardC c  n r w ws
--  , BackwardC c' n r w ws
--  -- substate
--  , ReplaceSubState s ss' s'
--  -- w
--  , Monoid w
--  -- Backward
--  , Default (BackwardArgC c)
--  , Default (BackwardArgC c')
--  -- assignments
--  , m  ~ MonadProgenitor mProgenitor s
--  , m' ~ MonadProgenitor mProgenitor s'
--  )
--  -- => Iso c m n r w ws s ss'
--  => Biparser c s m n r w ws u ss'
--  -> Biparser c' s' (MonadProgenitor mProgenitor s') n r w ws u v
--  -> Biparser c  s  (MonadProgenitor mProgenitor s)  n r w  ws u v
--zoomWrite (B.Biparser fw bw) (B.Biparser fw' bw') = B.Biparser
--  (StateErrorT \s -> do
--    (ss,s') <- runStateErrorT fw s
--    (x,_) <- changeMonad' @is () $ runStateErrorT fw' $ replaceSubState s ss
--    pure (x,s')
--  )
--  \u -> backwardT @c \r s -> do
--    (x,s',w)   <- runBackwardT @c' (bw' u) def r s
--    (_,s'',w') <- runBackwardT @c  (bw  u) def r s'
--    pure (x,s'', w <> w')
--
---- | Strict on the Iso which makes 'Biparser' slow to run if not all 'ss\'' is requred and for error to be thrown.
--zoomOne :: forall is c' mProgenitor w m' c s s' m n r ws u v i.
--  -- m
--  ( Monad m
--  , Alternative m
--  , ChangeMonad is m' m ()
--  --, ChangeFunction is m' m ~ ()
--  , MonadFail (StateErrorT i s m)
--  -- n
--  , Monad n
--  , BackwardC c  n r w ws
--  , BackwardC c' n r (Element w) ws
--  -- substate
--  , IsSequence (B.SubState s)
--  , ReplaceSubState s (Element (B.SubState s)) s'
--  , B.ElementContext c s
--  -- w
--  , MonoPointed w
--  -- Backward
--  , Default (BackwardArgC c')
--  -- assignments
--  , m  ~ MonadProgenitor mProgenitor s
--  , m' ~ MonadProgenitor mProgenitor s'
--  , i ~ ErrorContext m
--  )
--  => Biparser c' s' (MonadProgenitor mProgenitor s') n r (Element w) ws u v
--  -> Biparser c  s  (MonadProgenitor mProgenitor s)  n r w           ws u v
--zoomOne (B.Biparser fw' bw') = B.Biparser
--  (StateErrorT \s -> do
--    (ss,s') <- runStateErrorT @i (oneFw @c) s
--    (x,_) <- changeMonad' @is () $ runStateErrorT fw' $ replaceSubState s ss
--    pure (x,s')
--  )
--  \u -> backwardT @c \r s -> do
--    (x,s',w) <- runBackwardT @c' (bw' u) def r s
--    pure (x,s', singleton w)

