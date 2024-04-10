{-# LANGUAGE UndecidableInstances #-}
module Biparse.Generic.BinarySpec where

import Data.Word
import Biparse.Generic.Binary
import Data.Bits

spec :: Spec
spec = pure ()
--spec = do
--  fb @() "genericBinaryAdtIsoClass"
--    (genericBinaryAdtIsoClass @IndexContext)
--    ()
--    ()
--    (\fw -> do
--      it "
--    )

data ABC
  = A
  | B W8
  | C W16 W32
  deriving (Show, Eq, Generic)
instance IsoClass c m n a ABC where
  iso = genericBinaryAdtIsoClass

newtype W8 = W8 {unW8 :: Word8} deriving (Show,Eq)
instance SubElement a ~ Word8 => IsoClass c m n a W8 where
  iso = W8 <$> one `upon` unW8

newtype W16 = W16 {unW16 :: Word16} deriving (Show,Eq)
instance
  ( Monad m
  , Alt m
  , MonadWriter w n
  ) => IsoClass c m n a W16 where
  iso = do
    h <- (`shiftL` 8) . fromIntegral <$> one `upon` fromIntegral . (`shiftR` 8) . unW16
    l <- fromIntegral <$> one `upon` fromIntegral . unW16
    return $ W16 $ h .|. l

newtype W32 = W32 {unW32 :: Word32} deriving (Show,Eq)
instance
  ( IsoClass c m n a W16
  , Monad m
  , Alt m
  , MonadWriter w n
  ) => IsoClass c m n a W32 where
  iso = do
    h <- (`shiftL` 16) . fromIntegral . unW16 <$> iso `upon` W16 . fromIntegral . (`shiftR` 16) . unW32
    l <- fromIntegral . unW16 <$> iso `upon` W16 . fromIntegral . unW32
    return $ W32 $ h .|. l

