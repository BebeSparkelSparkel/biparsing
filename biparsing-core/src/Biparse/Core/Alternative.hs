{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE StandaloneKindSignatures #-}
module Biparse.Core.Alternative (
Alternative(..),
) where

import Data.Either (Either(Left,Right))
import Data.Maybe (Maybe)
import GHC.IO (IO)
import Control.Applicative qualified as A
import Biparse.Core.Aliases (LazyStateT, pattern LazyStateT, StrictStateT, LazyWriterT, pattern LazyWriterT, StrictWriterT, LazyRWST, pattern LazyRWST, StrictRWST)
import Control.Monad.Reader (ReaderT(ReaderT))
import Control.Arrow (Kleisli(Kleisli))
import Data.Functor.Compose (Compose(Compose))
import Data.Coerce (coerce)

import Control.Monad.Trans.Identity (IdentityT(IdentityT))
import Control.Monad.Trans.Writer qualified

infixl 3 <|>
class Alternative m where (<|>) :: m a -> m a -> m a

instance Alternative (Either a) where
  Right x <|> _ = Right x
  Left _ <|> x = x
instance Alternative Maybe where (<|>) = (A.<|>)
instance Alternative IO where (<|>) = (A.<|>)

instance Alternative m => Alternative (Kleisli m a) where Kleisli f <|> Kleisli g = Kleisli \x -> f x <|> g x
instance Alternative f => Alternative (Compose f g) where (<|>) = coerce ((<|>) :: f (g a) -> f (g a) -> f (g a)) :: forall a . Compose f g a -> Compose f g a -> Compose f g a

deriving instance Alternative m => Alternative (IdentityT m)
deriving via Kleisli m r instance Alternative m => Alternative (ReaderT r m)
--deriving via Kleisli (Compose m (Flip (,) s)) s instance (Alternative m, forall a b. CoercibleF m a b) => Alternative (LazyStateT s m)
instance Alternative m => Alternative (LazyStateT s m) where
  LazyStateT x <|> LazyStateT y = LazyStateT \s -> x s <|> y s
--deriving via Kleisli (Compose m (Flip (,) s)) s instance (Alternative m, CoercibleF m) => Alternative (StrictStateT s m)
----deriving via Compose m (Flip (,) w) instance (Alternative m, CoercibleF m) => Alternative (CPSWriterT w m)
--deriving via Compose m (Flip (,) w) instance (Alternative m, forall a b. Coercible (m (Flip (,) w a)) (m (Flip (,) w b))) => Alternative (LazyWriterT w m)
instance Alternative m => Alternative (LazyWriterT w m) where
  LazyWriterT x <|> LazyWriterT y = LazyWriterT (x <|> y)
--deriving via Compose m (Flip (,) w) instance (Alternative m, CoercibleF m) => Alternative (StrictWriterT w m)
----deriving instance (Alternative m, CoercibleF m) => Alternative (CPSRWST r w s m)
--deriving via (Kleisli (Kleisli (Compose m (Flip (Flip3 (,,) s) w)) s) r) instance (Alternative m, CoercibleF m) => Alternative (LazyRWST r w s m)
instance Alternative m => Alternative (LazyRWST r w s m) where
  LazyRWST x <|> LazyRWST y = LazyRWST \r s -> x r s <|> y r s
--deriving via (Kleisli (Kleisli (Compose m (Flip (Flip3 (,,) s) w)) s) r) instance (Alternative m, CoercibleF m) => Alternative (StrictRWST r w s m)
