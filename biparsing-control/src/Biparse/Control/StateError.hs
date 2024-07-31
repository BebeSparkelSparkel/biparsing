{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TupleSections #-}
module Biparse.Control.StateError (
StateErrorT(StateErrorT),
runStateErrorT,
) where

import Biparse.Core.Update (UpdateStateWithElement(updateStateWithElement))
import Control.Monad.State (StateT(StateT,runStateT), get, put, MonadState)
import Data.Sequences (IsSequence(uncons))
import Biparse.Core.Classes.Forward (try')

newtype StateErrorT s m a = StateErrorT' {unStateErrorT :: StateT s m a}
  deriving (Functor, Applicative, Monad, MonadTrans, Peek)

pattern StateErrorT x = StateErrorT' (StateT x)
{-# COMPLETE StateErrorT #-}

runStateErrorT :: StateErrorT s m a -> s -> m (a, s)
runStateErrorT (StateErrorT x) = x

deriving instance Monad m => MonadState s (StateErrorT s m)

instance Alt m => Alt (StateErrorT s m) where
  StateErrorT x <!> StateErrorT y = StateErrorT \s -> x s <!> y s

instance MonadError (s,e) m => MonadError e (StateErrorT s m) where
  throwError e = StateErrorT \s -> throwError (s,e)
  catchError (StateErrorT x) f = StateErrorT \s ->
    catchError (x s) $ uncurry $ flip $ runStateErrorT . f

instance (MonadError (s,e) m, IsString e) => MonadFail (StateErrorT s m) where
  fail msg = StateErrorT \s -> throwError (s, fromString msg)

instance (a ~ Element seq, UpdateStateWithElement s a, IsSequence seq, MonadError ((s,seq),e) m, IsString e) => OneFwd a (StateErrorT (s, seq) m) where
  oneFwd = oneFwd'

instance MonadError (s,e) m => Try (StateErrorT s m) where
  try = try'
--instance
--  ( IsSequence seq
--  , Monad m
--  , MonadFail (StateErrorT (s, seq) m)
--  , UpdateStateWithElement s a
--  , a ~ Element seq
--  ) => OneFwd a (StateErrorT (s, seq) m) where
--  oneFwd = do
--    (s, xs) <- get
--    (x, xs') <- maybe (fail "Unexpected end of input.") pure $ uncons xs
--    let s' = updateStateWithElement s x
--    put (s', xs')
--    return x
