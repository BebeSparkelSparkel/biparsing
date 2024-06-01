module Biparse.BinarySpec where

import Biparse.Binary
import Data.Binary
import Data.Binary.Put (execPut)
import Test.QuickCheck.Instances.ByteString ()
import Data.WideWord

spec :: Spec
spec = do
  test "word8" word8
  test "word16" word16
  test "word32" word32
  test "word64" word64
  test "word128" word128
  test "word256" word256
  test "int8" int8
  test "int16" int16
  test "int32" int32
  test "int64" int64
  test "int128" int128

test :: (Binary b, Arbitrary b, Eq b, Show b) => String -> Iso IndexContext IO IO () ByteStringBuilder () (IndexPosition LazyByteString) b -> Spec
test name bp = fb @IndexContext @(IndexPosition LazyByteString) @IO @IO name
  bp
  ()
  ()
  ()
  (\fw -> prop "same as binary" \ss -> case decodeOrFail ss of
    Right (remainder, i, x) -> fw (startIndex ss) `shouldReturn` (x, IndexPosition i remainder)
    Left _ -> fw (startIndex ss) `shouldThrow` isUserError
  )
  \bw -> prop "same as binary" \x -> bw x `shouldReturn` (x, execPut $ Data.Binary.put x)

instance Arbitrary Word128 where
  arbitrary = uncurry Word128 <$> arbitrary
  shrink = genericShrink
instance Arbitrary Word256 where
  arbitrary = arbitrary <&> to . coerce . from @(Word64,Word64,Word64,Word64)
  shrink = genericShrink
instance Arbitrary Int128 where
  arbitrary = uncurry Int128 <$> arbitrary
  shrink = genericShrink
