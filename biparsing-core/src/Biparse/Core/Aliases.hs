{-# LANGUAGE ViewPatterns #-}
module Biparse.Core.Aliases (
Biparser,
Iso,
Unit,
unit,
Const,
ConstU,

ReaderT(ReaderT),
LazyStateT,
StrictStateT,
CPSWriterT,
LazyWriterT,
StrictWriterT,
CPSRWST,
LazyRWST,
StrictRWST,

pattern LazyStateT,
pattern StrictStateT,
pattern CPSWriterT,
pattern LazyWriterT,
pattern StrictWriterT,
pattern CPSRWST,
pattern LazyRWST,
pattern StrictRWST,
) where

import Control.Monad.Trans.Reader (ReaderT(ReaderT))
import Control.Monad.Trans.State.Lazy qualified
import Control.Monad.Trans.State.Strict qualified
import Control.Monad.Trans.Writer.CPS qualified
import Control.Monad.Trans.Writer.Lazy qualified
import Control.Monad.Trans.Writer.Strict qualified
import Control.Monad.Trans.RWS.CPS qualified
import Control.Monad.Trans.RWS.Lazy qualified
import Control.Monad.Trans.RWS.Strict qualified

-- | Alias just to indicate that the profunctor is a biparser
type Biparser :: (Type -> Type -> Type) -> Type -> Type -> Type
type Biparser p = p

-- | Iso when @u ~ v@
type Iso :: (Type -> Type -> Type) -> Type -> Type
type Iso p v = Biparser p v v

-- | Unit when @u@ and @v@ are @()@
type Unit p = Biparser p () ()

-- | Throws away @u@ and @v@
unit :: forall p u. Profunctor p => Unit p -> Const p u
unit = lmap $ const ()

-- | Discards @u@ and returns ()
type Const p u = Biparser p u ()

-- | Discards @u@
type ConstU p u v = Biparser p u v


type LazyStateT    = Control.Monad.Trans.State.Lazy.StateT
type StrictStateT  = Control.Monad.Trans.State.Strict.StateT
type CPSWriterT    = Control.Monad.Trans.Writer.CPS.WriterT
type LazyWriterT   = Control.Monad.Trans.Writer.Lazy.WriterT
type StrictWriterT = Control.Monad.Trans.Writer.Strict.WriterT
type CPSRWST       = Control.Monad.Trans.RWS.CPS.RWST
type LazyRWST      = Control.Monad.Trans.RWS.Lazy.RWST
type StrictRWST    = Control.Monad.Trans.RWS.Strict.RWST

pattern LazyStateT :: (s -> m (a, s)) -> LazyStateT s m a
pattern LazyStateT    x = Control.Monad.Trans.State.Lazy.StateT x
pattern StrictStateT :: (s -> m (a, s)) -> StrictStateT s m a
pattern StrictStateT  x = Control.Monad.Trans.State.Strict.StateT x
pattern CPSWriterT :: (Functor m, Monoid w) => () => m (a, w) -> CPSWriterT w m a
pattern CPSWriterT    x <- (Control.Monad.Trans.Writer.CPS.runWriterT -> x) where
  CPSWriterT x = Control.Monad.Trans.Writer.CPS.writerT x
pattern LazyWriterT :: m (a, w) -> LazyWriterT w m a
pattern LazyWriterT   x = Control.Monad.Trans.Writer.Lazy.WriterT x
pattern StrictWriterT :: m (a, w) -> StrictWriterT w m a
pattern StrictWriterT x = Control.Monad.Trans.Writer.Strict.WriterT x
pattern CPSRWST :: (Functor m, Monoid w) => () => (r -> s -> m (a, s, w)) -> CPSRWST r w s m a
pattern CPSRWST       x <- (Control.Monad.Trans.RWS.CPS.runRWST -> x) where
  CPSRWST x = Control.Monad.Trans.RWS.CPS.rwsT x
pattern LazyRWST :: (r -> s -> m (a, s, w)) -> LazyRWST r w s m a
pattern LazyRWST      x = Control.Monad.Trans.RWS.Lazy.RWST x
pattern StrictRWST :: (r -> s -> m (a, s, w)) -> StrictRWST r w s m a
pattern StrictRWST    x = Control.Monad.Trans.RWS.Strict.RWST x
{-# COMPLETE LazyStateT #-}
{-# COMPLETE StrictStateT #-}
{-# COMPLETE CPSWriterT #-}
{-# COMPLETE LazyWriterT #-}
{-# COMPLETE StrictWriterT #-}
{-# COMPLETE CPSRWST #-}
{-# COMPLETE LazyRWST #-}
{-# COMPLETE StrictRWST #-}
