{-|
Parse the fields of type 'b' out of their order in the Constructor.
-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE GADTs #-}
module Biparse.Unordered
  ( unorderedBiparserDef
  , unorderedBiparser
  , Accumulating(..)
  , Optional(..)
  , MakeWriter
  , UnorderedParser
  ) where

import Biparse.Biparser (Iso, IsoClass(iso), SubState, upon, try, forward, backward, Biparser(Biparser))
import Biparse.List (many, Many)
import GHC.Generics (Rec0, (:*:)((:*:)), M1(M1,unM1), K1(K1,unK1), Generic(Rep,to,from), D1, Meta(MetaData))
import Biparse.General (optional)
import Data.Proxy (Proxy(Proxy))
import Data.Kind (Constraint)
import Data.HVect (HVect((:&:)), Append, (<++>))
import Data.HVect qualified as HV
import GHC.IO (IO)
import Data.IORef (IORef, newIORef, writeIORef, modifyIORef, readIORef)
import System.IO.Unsafe (unsafePerformIO)
import GHC.TypeLits (Symbol, KnownSymbol, AppendSymbol, symbolVal)
import Data.MonoTraversable.Unprefixed (all)
import Data.Default (Default(def))

unorderedBiparserDef :: forall c m n a b.
  ( MakeWriter c m n a (Rep b)
  , UnorderedParser c m n a b
  , Default b
  , Monad n
  ) => Iso c m n a b
unorderedBiparserDef = unorderedBiparser def

unorderedBiparser :: forall c m n a b.
  ( MakeWriter c m n a (Rep b)
  , UnorderedParser c m n a b
  , Monad n
  )
  => b
  -> Iso c m n a b
unorderedBiparser x = Biparser
  do
    y <- pure $ pure x
    unorderdParser @c @m @n @a @b y
  (backward $ to <$> makeWriter @c @m @n @a `upon` from)

-- | Helper Type Wrappers

-- | Type wrapper used to accumulate the results in a collection of the parser that could be seperated by different parsed fields.
newtype Accumulating a = Accumulating {unAccumulating :: a} deriving (Show, Eq, Ord)
instance
  ( IsoClass c m n a (Element b)
  , IsSequence b
  , Many c a m n
  ) => IsoClass c m n a (Accumulating b) where
  iso = Accumulating . fromList <$> many iso `upon` toList . unAccumulating

-- | Type wrapper used to signal that the field parser is not required to succeed for the type parse succeed.
newtype Optional a = Optional {unOptional :: Maybe a} deriving (Show, Eq, Ord)
instance
  ( IsoClass c m n a b
  , MonadPlus m
  , MonadState a m
  , Monad n
  , Alternative n
  , Monoid (SubState c a)
  ) => IsoClass c m n a (Optional b) where
  iso = Optional <$> optional (iso @c @m @n @a @b) `upon` unOptional

-- | Parser

unorderdParser :: forall c m n a b.
  UnorderedParser c m n a b
  => IO b
  -> m b
unorderdParser x = do
  (embededIORefs, parsers) <- pure . unsafePerformIO $ addIORefs . from =<< x
  remainingParsers <- untilNothingM runParsers $ makeParsers @c @m @n @a parsers
  if all isComplete remainingParsers
    then pure $ to $ unsafePerformIO $ lowerG $ mapG @_ @(Rep b) readIORef embededIORefs
    else fail $ "Could not unordered parse " <> typeName @b
  where
  untilNothingM :: forall m' a'. Monad m' => (a' -> m' (Maybe a')) -> a' -> m' a'
  untilNothingM f x' = maybe (pure x') (untilNothingM f) =<< f x'

type UnorderedParser c m n a b =
  ( Generic b
  , MakeParsers c m n a (Refs (Rep b))
  , AddIORefs (Rep b)
  , MonadFail m
  , LowerG IO (Rep b)
  , MapG IORef (Rep b)
  , KnownSymbol (TypeName (Rep b))
  )

-- | Quantified Constraint Trick for 'LiftG m (Rep a)'
-- Reference: https://blog.poisson.chat/posts/2022-09-21-quantified-constraint-trick.html
class LiftG m (Rep a) => LiftGRep m a
instance LiftG m (Rep a) => LiftGRep m a

data ParserType a
  = SingleSuccessParser a
  | AccumulatingParser a
  | OptionalParser a
getParser :: ParserType a -> a
getParser = \case
  SingleSuccessParser x -> x
  AccumulatingParser x -> x
  OptionalParser x -> x
isComplete :: ParserType a -> Bool
isComplete = \case
  AccumulatingParser _ -> True
  OptionalParser _ -> True
  _ -> False

runParsers :: forall m ps. (Monad m, ps ~ [ParserType (m Bool)]) => ps -> m (Maybe ps)
runParsers = \case
  p : ps -> getParser p >>= bool (cons p <$$> runParsers ps) (pure $ Just case p of
    AccumulatingParser _ -> p : ps
    _ -> ps
    )
  [] -> pure Nothing

typeName :: forall a. KnownSymbol (TypeName (Rep a)) => String
typeName = symbolVal $ Proxy @(TypeName (Rep a))
type (++) :: Symbol -> Symbol -> Symbol
type family (++) a b where a ++ b = AppendSymbol a b
type TypeName :: (Type -> Type) -> Symbol
type family TypeName a where TypeName (D1 ('MetaData dataName moduleName packageName _) _) = packageName ++ "." ++ moduleName ++ "." ++ dataName

type Refs :: forall {k}. (k -> Type) -> [Type]
type family Refs f where
  Refs (M1 _ _ f) = Refs f
  Refs (f :*: g) = Append (Refs f) (Refs g)
  Refs (Rec0 (Accumulating a)) = '[ IORef (Accumulating a) ]
  Refs (Rec0 a) = '[ IORef a ]

class AddIORefs (f :: Type -> Type) where
  addIORefs :: f p -> IO (Lifted IORef (LiftType IORef f p), HVect (Refs f))
instance AddIORefs f => AddIORefs (M1 i c f) where
  addIORefs = fmap (first $ Lifted . M1 . runLifted) . addIORefs . unM1
instance (AddIORefs f, AddIORefs g) => AddIORefs (f :*: g) where
  addIORefs (x :*: y) = do
    (Lifted x', x'') <- addIORefs x
    (Lifted y', y'') <- addIORefs y
    pure (Lifted $ x' :*: y', x'' <++> y'')
instance {-# OVERLAPPABLE #-} AddIORefs (Rec0 a) where
  addIORefs (K1 x) = do
    r <- newIORef x
    pure (Lifted $ K1 r, HV.singleton r)

class MakeParsers c m n a bs where
  makeParsers :: HVect bs -> [ParserType (m Bool)]
instance {-# OVERLAPS #-}
  ( IsoClass c m n a (Element b)
  , SemiSequence b
  , MonadPlus m
  , MonadState a m
  , MakeParsers c m n a bs
  ) => MakeParsers c m n a (IORef (Accumulating b) ': bs) where
  makeParsers (r :&: rs)
    = AccumulatingParser ( do
          x <- forward $ try $ iso @c @m @n @a
          () <- pure $ unsafePerformIO $ modifyIORef r $ Accumulating . cons x . unAccumulating
          pure True
      <|> pure False
      )
    : makeParsers @c @m @n @a rs
instance {-# OVERLAPPABLE #-}
  ( IsoClass c m n a b
  , MakeParsers c m n a bs
  , MonadPlus m
  , MonadState a m
  ) => MakeParsers c m n a (IORef b ': bs) where
  makeParsers (r :&: rs)
    = SingleSuccessParser ( do
          x <- forward $ try $ iso @c @m @n @a
          () <- pure $ unsafePerformIO $ writeIORef r x
          pure True
      <|> pure False
      )
    : makeParsers @c @m @n @a rs
instance MakeParsers c m n a '[] where
  makeParsers = mempty

-- * Writer

-- | Used to create the Generic writer.
class MakeWriter c m n a f where
  makeWriter :: Iso c m n a (f p)
instance
  ( MakeWriter c m n a f
  , Monad n
  , Functor m
  ) => MakeWriter c m n a (M1 i c' f) where
  makeWriter = M1 <$> makeWriter @c @m @n `upon` unM1
instance
  ( MakeWriter c m n a f
  , MakeWriter c m n a g
  , MonadWriter ss n
  , Monad m
  , ss ~ SubState c a
  ) => MakeWriter c m n a (f :*: g) where
  makeWriter
    =   (:*:)
    <$> makeWriter @c @m @n @a @f `upon` (\(x :*: _) -> x)
    <*> makeWriter @c @m @n @a @g `upon` (\(_ :*: x) -> x)
instance
  ( IsoClass c m n a b
  , Monad n
  , Functor m
  ) => MakeWriter c m n a (Rec0 b) where
  makeWriter = K1 <$> iso @c @m @n @a `upon` unK1

-- * Modify the Generic data typs

type Lifted :: (Type -> Type) -> Type -> Type
newtype Lifted m a = Lifted {runLifted :: a} deriving (Functor)

type LiftType :: forall {k}. (Type -> Type) -> (k -> Type) -> k -> Type
type family LiftType m f where
  LiftType m (M1 i c f) = M1 i c (LiftType m f)
  LiftType m (f :*: g) = LiftType m f :*: LiftType m g
  LiftType m (K1 i c) = K1 i (m c)

type LiftG :: forall {k}. (Type -> Type) -> (k -> Type) -> Constraint
class LiftG m f where
  liftG  :: forall {p}. f p -> Lifted m (LiftType m f p)
instance (LiftG m f) => LiftG m (M1 i c f) where
  liftG = Lifted . M1 . runLifted . liftG @m @f . unM1
instance (LiftG m f, LiftG m g) => LiftG m (f :*: g) where
  liftG (x :*: y) = Lifted $ runLifted @m (liftG x) :*: runLifted @m (liftG y)
instance Applicative m => LiftG m (K1 i c) where
  liftG = Lifted . K1 . pure . unK1

type LowerG :: forall {k}. (Type -> Type) -> (k -> Type) -> Constraint
class LowerG m f where
  lowerG :: forall {p}. Lifted m (LiftType m f p) -> m (f p)
instance (Functor m, LowerG m f) => LowerG m (M1 i c f) where
  lowerG = fmap M1 . lowerG . fmap unM1
instance (Applicative m, LowerG m f, LowerG m g) => LowerG m (f :*: g) where
  lowerG (Lifted (x :*: y)) = (:*:) <$> lowerG (Lifted x) <*> lowerG (Lifted y)
instance Applicative m => LowerG m (K1 i c) where
  lowerG = fmap K1 . unK1 . runLifted

type MapG :: forall {k}. (Type -> Type) -> (k -> Type) -> Constraint
class MapG m f where
  mapG   :: forall n {p}. (forall a. m a -> n a) -> Lifted m (LiftType m f p) -> Lifted n (LiftType n f p)
instance MapG m f => MapG m (M1 i c f) where
  mapG f = fmap M1 . mapG @_ @f f . fmap unM1
instance (MapG m f, MapG m g) => MapG m (f :*: g) where
  mapG f (Lifted (x :*: y)) = Lifted $ runLifted (mapG @_ @f f (Lifted x)) :*: runLifted (mapG @_ @g f (Lifted y))
instance MapG m (K1 i c) where
  mapG f = Lifted . K1 . f . unK1 . runLifted
