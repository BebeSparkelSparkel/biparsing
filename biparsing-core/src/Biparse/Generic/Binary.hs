{-# LANGUAGE UndecidableInstances #-}
module Biparse.Generic.Binary
  ( genericBinaryAdtIsoClass
  ) where

import Biparse.Biparser (Iso, IsoClass(iso), SubState, SubElement, GetSubState, UpdateStateWithElement, pattern Biparser, upon)
import Biparse.General (take)
import GHC.Generics (Generic(Rep,to,from), D1, C1, S1, M1(M1,unM1), K1(K1,unK1), Rec0, (:*:)((:*:)), (:+:)(L1,R1), )
import Control.Monad.State (State, evalState)

-- | Define IsoClass for Summed (multiple constructors) ADTs.
-- Cannot be used with (<|>) or (<!>) since no type header, only a constructor header, is defined to differentiate types.
-- The constructor is preceeded by a enumerated element.
genericBinaryAdtIsoClass :: forall c m n a b.
  ( Generic b
  , GenericBinaryAdtIsoClass c (Rep b) m n a
  , Functor m
  , Monad n
  ) => Iso c m n a b
genericBinaryAdtIsoClass = to <$> evalState (genericBinaryAdtIsoClass' @c) 0 `upon` from

class GenericBinaryAdtIsoClass c b m n a where
  genericBinaryAdtIsoClass' :: State Int (Iso c m n a (b p))
instance
  ( GenericBinaryAdtIsoClass c cons m n a
  , GenericBinaryAdtIsoClass c cons' m n a
  , Alt m
  , Monad n
  ) => GenericBinaryAdtIsoClass c (D1 meta (cons :+: cons')) m n a where
  genericBinaryAdtIsoClass' = do
    x <- genericBinaryAdtIsoClass' @c @(cons :+: cons')
    return $ M1 <$> x `upon` unM1
instance
  ( GenericBinaryAdtIsoClass c cons m n a
  , GenericBinaryAdtIsoClass c cons' m n a
  , Alt m
  , Functor n
  ) => GenericBinaryAdtIsoClass c (cons :+: cons') m n a where
  genericBinaryAdtIsoClass' = do
    -- Identity does not have MonadFail so this stupid syntax is required
    (fw,bw) <- genericBinaryAdtIsoClass' @c @cons @_ @_ @a <&> \(Biparser f b) -> (f,b)
    (fw',bw') <- genericBinaryAdtIsoClass' @c @cons' @_ @_ @a <&> \(Biparser f b) -> (f,b)
    return $ Biparser (L1 <$> fw <!> R1 <$> fw') \case
      L1 x -> L1 <$> bw x
      R1 x -> R1 <$> bw' x
instance
  ( ProductIsoClass c sels m n a
  , Enum se
  , Eq se
  , Show se
  , MonadState a m
  , MonadError e m
  , MonadFail m
  , Alt m
  , MonadWriter w n
  , MonadFail n
  , ConvertElement c se w n
  , GetSubState a
  , UpdateStateWithElement c a
  , IsSequence ss
  , ss ~ SubState a
  , se ~ SubElement a
  ) => GenericBinaryAdtIsoClass c (C1 meta sels) m n a where
  genericBinaryAdtIsoClass' = do
    prefix <- get
    put $ succ prefix
    return do
      take $ toEnum prefix
      M1 <$> productIsoClass @c @sels `upon` unM1

-- | Parses the fields of a record on after the other.
-- Careful if the IsoClass of a field does not consume a fixed amount because it could start parsing data of the next field.
class ProductIsoClass c b m n a where
  productIsoClass :: Iso c m n a (b p)
instance (ProductIsoClass c s m n a, ProductIsoClass c s' m n a, Monad m, Monad n) => ProductIsoClass c (s :*: s') m n a where
  productIsoClass = do
    x <- productIsoClass @c @s `upon` \(x :*: _) -> x
    y <- productIsoClass @c @s' `upon` \(_ :*: y) -> y
    return $ x :*: y
instance (ProductIsoClass c r m n a, Functor m, Monad n) => ProductIsoClass c (S1 meta r) m n a where
  productIsoClass = M1 <$> productIsoClass @c @r `upon` unM1
instance (IsoClass c m n a b, Functor m, Monad n) => ProductIsoClass c (Rec0 b) m n a where
  productIsoClass = K1 <$> iso @c `upon` unK1
  
