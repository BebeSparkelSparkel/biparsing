{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Biparse.Binary.Generics
  ( genericBinaryAdtIsoClass
  ) where

import Control.Monad.State.Lazy (State, get, put, evalState)
import GHC.Generics (Generic(Rep,to,from), D1, C1, S1, U1(U1), M1(M1,unM1), K1(K1,unK1), Rec0, (:*:)((:*:)), (:+:)(L1,R1))

-- | Define IsoClass for Summed (multiple constructors) ADTs.
-- Cannot be used with (<|>) or (<|>) since no type header, only a constructor header, is defined to differentiate types.
-- The constructor is preceeded by a enumerated element.
genericBinaryAdtIsoClass :: forall v p a.
  ( Generic v
  , GenericBinaryAdtIsoClass (Rep v) p a
  , Profunctor p
  , forall u. Functor (p u)
  ) => Iso p v
genericBinaryAdtIsoClass = to <$> evalState (genericBinaryAdtIsoClass' @_ @_ @a) 0 `upon` from

class GenericBinaryAdtIsoClass b p a where
  genericBinaryAdtIsoClass' :: State Int (Iso p (b a))
instance
  ( GenericBinaryAdtIsoClass cons  p a
  , GenericBinaryAdtIsoClass cons' p a
  , Profunctor p
  , forall u. Monad (p u)
  , ComapM p m
  , forall u. Alternative (p u)
  , MonadFail m
  ) => GenericBinaryAdtIsoClass (D1 meta (cons :+: cons')) p a where
  genericBinaryAdtIsoClass' = do
    x <- genericBinaryAdtIsoClass' @(cons :+: cons')
    return $ M1 <$> x `upon` unM1
instance
  ( GenericBinaryAdtIsoClass cons  p a
  , GenericBinaryAdtIsoClass cons' p a
  , Profunctor p
  , ComapM p m
  , MonadFail m
  , forall u. Alternative (p u)
  , forall u. Monad (p u)
  ) => GenericBinaryAdtIsoClass (cons :+: cons') p a where
  genericBinaryAdtIsoClass' = do
    -- Identity does not have MonadFail so this stupid syntax is required
    l <- genericBinaryAdtIsoClass' @cons  @_ @a
    r <- genericBinaryAdtIsoClass' @cons' @_ @a
    return $ L1 <$> l `uponMay` l1  <|> R1 <$> r `upon` r1
    where
    l1 = \case L1 x -> Just x; _ -> Nothing
    r1 (R1 x) = x
instance
  ( ProductIsoClass (sel :*: sel') p
  , One Word8 (p Word8)
  , Profunctor p
  , forall u. Try (p u)
  , forall u. MonadFail (p u)
  ) => GenericBinaryAdtIsoClass (C1 meta (sel :*: sel')) p a where
  genericBinaryAdtIsoClass' = do
    prefix <- get
    put $ succ prefix
    return do
      take (toEnum prefix :: Word8)
      M1 <$> productIsoClass @(sel :*: sel') `upon` unM1
instance
  ( IsoClass b p
  , One Word8 (p Word8)
  , Profunctor p
  , forall u. MonadFail (p u)
  , forall u. Try (p u)
  ) => GenericBinaryAdtIsoClass (C1 meta (S1 meta' (Rec0 b))) p a where
  genericBinaryAdtIsoClass' = do
    prefix <- get
    put $ succ prefix
    return do
      take (toEnum prefix :: Word8)
      M1 . M1 . K1 <$> iso `upon` unK1 . unM1 . unM1
instance 
  ( ComapM p m
  , One Word8 (p Word8)
  , forall u. MonadFail (p u)
  , forall u. Try (p u)
  , MonadFail m
  ) => GenericBinaryAdtIsoClass (C1 meta U1) p a where
  genericBinaryAdtIsoClass' = do
    prefix <- get
    put $ succ prefix
    return $ takeDi (toEnum prefix :: Word8) (M1 U1 :: C1 meta U1 a)

-- | Parses the fields of a record on after the other.
-- Careful if the IsoClass of a field does not consume a fixed amount because it could start parsing data of the next field.
class ProductIsoClass b p where
  productIsoClass :: forall a. Iso p (b a)
instance (ProductIsoClass s p, ProductIsoClass s' p, Profunctor p, forall u. Monad (p u)) => ProductIsoClass (s :*: s') p where
  productIsoClass = do
    x <- productIsoClass @s `upon` \(x :*: _) -> x
    y <- productIsoClass @s' `upon` \(_ :*: y) -> y
    return $ x :*: y
instance (ProductIsoClass r p, Profunctor p, forall u. Monad (p u)) => ProductIsoClass (S1 meta r) p where
  productIsoClass = M1 <$> productIsoClass @r `upon` unM1
instance (IsoClass b p, Profunctor p, forall u. Monad (p u)) => ProductIsoClass (Rec0 b) p where
  productIsoClass = K1 <$> iso `upon` unK1
  
