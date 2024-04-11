{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverloadedLists #-}
module Biparse.Binary.GenericsSpec where

--import Data.Word
import Biparse.Binary
import Biparse.Binary.Generics
--import Data.Bits

spec :: Spec
spec = do
  fb "genericBinaryAdtIsoClass"
    (genericBinaryAdtIsoClass :: Iso IndexContext IO IO () BuilderByteString () (IndexPosition LazyByteString) ABC)
    ()
    ()
    (\fw -> do
      it "A" $ fw (startIndex [0]) `shouldReturn` (A, IndexPosition 1 [])
      it "B" $ fw (startIndex [1,2]) `shouldReturn` (B 2, IndexPosition 2 [])
      it "C" $ fw (startIndex [2,0,1,255,255,255,255,0]) `shouldReturn` (C 1 maxBound, IndexPosition 7 [0])
    )
    \bw -> do
      it "A" $ bw A `shouldReturn` (A, [0])
      it "B" $ bw (B 5) `shouldReturn` (B 5, [1,5])
      it "C" $ bw (C 0x0102 0x03040506) `shouldReturn` (C 0x0102 0x03040506, [2,1,2,3,4,5,6])

data ABC
  = A
  | B (Bin Word8)
  | C (Bin Word16) (Bin Word32)
  deriving (Show, Eq, Generic)

