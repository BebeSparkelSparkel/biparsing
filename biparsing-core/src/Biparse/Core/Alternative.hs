{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE StandaloneKindSignatures #-}
module Biparse.Core.Alternative (
Alternative(..),
) where

import Biparse.Core.Aliases (ReaderT(ReaderT), CPSWriterT, pattern CPSWriterT, LazyWriterT, pattern LazyWriterT, StrictWriterT, pattern StrictWriterT, LazyStateT, pattern LazyStateT, StrictStateT, pattern StrictStateT, CPSRWST, pattern CPSRWST, LazyRWST, pattern LazyRWST, StrictRWST, pattern StrictRWST)
import Control.Applicative qualified as A
import Control.Arrow (Kleisli(Kleisli))
import Control.Monad.Trans.Identity (IdentityT(IdentityT))
import Data.Either (Either(Left,Right))
import Data.Functor (Functor)
import Data.Maybe (Maybe)
import Data.Monoid (Monoid)
import GHC.IO (IO)

infixl 3 <|>
class Alternative m where (<|>) :: m a -> m a -> m a

instance Alternative (Either a) where
  Right x <|> _ = Right x
  Left _ <|> x = x
instance Alternative Maybe where (<|>) = (A.<|>)
instance Alternative IO where (<|>) = (A.<|>)

instance Alternative m => Alternative (Kleisli m a) where Kleisli f <|> Kleisli g = Kleisli \x -> f x <|> g x

deriving instance Alternative m => Alternative (IdentityT m)

deriving via Kleisli m r instance Alternative m => Alternative (ReaderT r m)

instance (Functor m, Alternative m, Monoid w) => Alternative (CPSWriterT w m) where CPSWriterT x <|> CPSWriterT y = CPSWriterT (x <|> y)
instance Alternative m => Alternative (LazyWriterT w m) where LazyWriterT x <|> LazyWriterT y = LazyWriterT (x <|> y)
instance Alternative m => Alternative (StrictWriterT w m) where StrictWriterT x <|> StrictWriterT y = StrictWriterT (x <|> y)

instance Alternative m => Alternative (LazyStateT s m) where LazyStateT x <|> LazyStateT y = LazyStateT \s -> x s <|> y s
instance Alternative m => Alternative (StrictStateT s m) where StrictStateT x <|> StrictStateT y = StrictStateT \s -> x s <|> y s

instance (Functor m, Alternative m, Monoid w) => Alternative (CPSRWST r w s m) where CPSRWST x <|> CPSRWST y = CPSRWST \r s -> x r s <|> y r s
instance Alternative m => Alternative (LazyRWST r w s m) where LazyRWST x <|> LazyRWST y = LazyRWST \r s -> x r s <|> y r s
instance Alternative m => Alternative (StrictRWST r w s m) where StrictRWST x <|> StrictRWST y = StrictRWST \r s -> x r s <|> y r s
