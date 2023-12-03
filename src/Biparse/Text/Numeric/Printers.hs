-- | Ryu: Fast Float-to-String Conversion
-- PLDI’18, June 18ś22, 2018, Philadelphia, PA,SA
-- C� 2018 Copyright held by the owner/author(s).
-- ACM ISBN 978-1-4503-5698-5/18/06.
-- https://doi.org/10.1145/3192366.3192369
module Biparser.Text.Numeric.Pringers
  ( ryu
  )

--import Data.Bits (finiteBitSize)
--import Data.Vector.Mutable qualified as VM
--import Data.Vector qualified as V
--
---- | Ryu 32 Bit Float Implementation
---- DEV NOTE: Once working shiftL and shiftR can use the unsafe functions
---- DEV NOTE: try using unsafeNew instead new for result vector
--ryu :: forall f. f ~ Float => Float -> String
--ryu = toWord >>> \bits -> let
--  sign = (bits `shiftR` (numberMantissaBits @f + numberExponentBits @f) .&. 1) /= 0
--  ieeeMantissa = bits .&. mantissaMask
--  ieeeExponent = (bits `shiftR` numberMantissaBits @f) .&. ((1 `shiftL` numberExponentBits @f) - 1)
--  in if ieeeExponent == mantissaMask || (ieeeExponent == 0 && ieeeMantissa == 0
--    then _special_string
--    else
--
--floatingDecimal32 = let
--  (e2, m2) = if ieeeExponent == 0
--    then (1 - floatBias - floatMantissaBits - 2, ieeeMantissa)
--    else (ieeeExponent - floatBias - floatMantissaBits - 2, 1 `shiftL` floatMantissaBits .|. ieeeMantissa)
--
--  -- even = m2 .&. 1 == 0
--  -- acceptBounds = even
--  acceptBounds = m2 .&. 1 == 0
--
--  mv = 4 * m2
--  mp = 4 * m2 + 2o
--  mmShift = ieeeMantissa /= 0 || ieeeExponent <= 1
--  mm = 4 * m2 - 1 - mmShift
--
--  in runST do
--    vr <- newSTRef 0
--    vp <- newSTRef 0
--    vm <- newSTRef 0
--    e10 <- newSTRef 0
--    vmIsTrailingZeros <- newSTRef False
--    vrIsTrailingZeros <- newSTRef False
--    lastRemovedDigit <- newSTRef 0
--
--    if e2 >= 0
--    then do
--      let q = log10Pow2 e2
--      writeSTRef e10 q
--      let k = floatPow5InvBitCount + pow5bits q - 1
--          i = negate e2 + q + k
--      writeSTRef vr $ mulPow5divPow2 mr q i
--      writeSTRef vp $ mulPow5divPow2 mp q i
--      writeSTRef vm $ mulPow5divPow2 mm q i
--      when (readSTRef vp >>= \vp' -> readSTRef vm <&> \vm' -> q /= 0 && (vp' - 1) `div` ten <= vm' `div` ten) do
--        let l = floatPow5InvBitCount + pow5bits (q - 1) - 1
--        writeSTRef lastRemovedDigit $ mulPow5InvDivPow2 mv (q - 1) (negate ew + q - 1 + l) `mod` ten
--      when (q <= 9)
--        if mv `mod` 5 == 0
--        then writeSTRef vrIsTrailingZeros $ multipleOfPowerOf5_32 mv q
--        else if acceptBounds
--        then writeSTRef vmIsTrailingZeros $ multipleOfPowerOf5_32 mm 1
--        else modifySTRef vp (- multipleOfPowerOf5_32 mp q
--    else do
--      let q = log10Pow5 $ negate e2
--      writeSTRef e10 $ q + e2
--      let i = negate e2 - q
--          k = pow5bits i - floatPow5BitCount
--      j <- newSTRef $ q - k
--      writeSTRef vr $ mulPow5divPow2 mr i j
--      writeSTRef vp $ mulPow5divPow2 mp i j
--      writeSTRef vm $ mulPow5divPow2 mm i j
--      when (readSTRef vp >>= \vp' -> readSTRef vm <&> \vm' -> q /= 0 && (vp' - 1) `div` ten <= vm' `div` ten) do
--        writeSTRef j $ q - 1 - (pow5bits (i + 1) - floatPow5BitCount
--        writeSTRef lastRemovedDigit =<< (`mod` ten) . mulPow5divPow2 mv (i + 1) (readSTRef j)
--      if q <= 1
--      then do
--        writeSTRef vrIsTrailingZeros True
--        if acceptBounds
--        then writeSTRef vmIsTrailingZeros $ mmShift == 1
--        else modifySTRef vp prev
--      else when (q < 31) $ modifySTRef vrIsTrailingZeros $ multipleOfPowerOf2_32 mv $ prev q
--
--    removed <- newSTRef 0
--    output <- newSTRef 0
--    ifM ((||) <$> readSTRef vmIsTrailingZeros <*> readSTRef vrIsTrailingZeros)
--      do
--        whileM_ (readSTRef vp \vp' -> readSTRef vm <&> \vm' -> vp' `div` ten > vm `div` ten) do
--          modifySTRef vmIsTrailingZeros =<< readSTRef vm <&> \vm' -> (&& vm' `mod` 10 == 0)
--          modifySTRef vrIsTrailingZeros =<< readSTRef lastRemovedDigit <&> \lastRemovedDigit' -> (&& lastRemovedDigit == 0)
--          writeSTRef lastRemovedDigit =<< readSTRef vr <$> (`mod` ten)
--          modifySTRef vr (`div` ten)
--          modifySTRef vp (`div` ten)
--          modifySTRef vm (`div` ten)
--          modifySTRef removed succ
--        whenM (readSTRef vmIsTrailingZeros) $
--          whileM_ (readSTRef vm <&> \vm' -> vm' `mod` ten == 0) do
--            modifySTRef vrIsTrailingZeros =<< readSTRef lastRemovedDigit <&> \lastRemovedDigit' -> (&& lastRemovedDigit == 0)
--            writeSTRef lastRemovedDigit =<< readSTRef vr <$> (`mod` ten)
--            modifySTRef vr (`div` ten)
--            modifySTRef vp (`div` ten)
--            modifySTRef vm (`div` ten)
--            modifySTRef removed succ
--        whenM (readSTRef vrIsTrailingZeros >>= \vrIsTrailingZeros' -> readSTRef lastRemovedDigit >>= \lastRemovedDigit' -> vr <&> \vr' ->
--              vrIsTrailingZeros' && lastRemovedDigit' == 5 && vr `mod` 2 == 0) $
--          writeSTRef lastRemovedDigit 4
--        writeSTRef output =<< (readSTRef vr >>= \vr' -> readSTRef vm >>= \vm' -> readSTRef vmIsTrailingZeros >>= \vmIsTrailingZeros' -> readSTRef lastRemovedDigit <&> \lastRemovedDigit' ->
--          if vr == vm && not (acceptBounds && vmIsTrailingZeros)
--
--toChars =
--    runST do
--      result <- VM.new $ charBufferSize @f
--      index <- newSTRef 0
--
--      ifM sign do
--        write result 0 '-'
--        modifySTRef index succ
--
--      output <- newSTRef ieeeMantissa
--      let olength = decimalLength9 ieeeMantissa
--
--      i <- newSTRef 0
--      whileM (readSTRef output <&> (>= tenThousand)) do
--        c <- readSTRef output <&> (`mod` tenThousand)
--        modifySTRef output (`div` tenThousand)
--        c0 <- (`shiftL` 1) . (`mod` oneHundred) <$> readSTRef c
--        c1 <- (`shiftL` 1) . (`div` oneHundred) <$> readSTRef c
--        let i' = result + index + olength - i
--        memcpy2 result (i' - 1) c0
--        memcpy2 result (i' - 3) c1
--        modifySTRef i (+ 4)
--      whenM (readSTRef output <&> (>= oneHundred)) do
--        c <- readSTRef output <&> (`shiftL` 1) . (`mod` oneHundred)
--        modifySTRef output (`div` oneHundred)
--        memcpy2 (result + index + olength - i - 1) c
--        modifySTRef i (+ 2)
--      ifM (readSTRef output <&> (>= ten))
--        do
--          c <- readSTRef output <&> (`shiftL` 1)
--          write result (index + olength - i) $ digitTable ! c + 1
--          write result index $ digitTable ! c
--        $ write result index =<< ('0' +) <$> readSTRef output
--
--  where
--  mantissaMask = 1 `shiftL` numberExponentBits @f - 1
--
--toWord :: a -> SameSizeWord a
--type SameSizeWord :: Type -> Type
--type family SameSizeWord a
--instance SameSizeWord Float = Word32
--
--ten = 10
--oneHundred = 100
--tenThousand = 10000
--
--decimalLength9 v
--  -- assert(v < 1000000000);
--  | v         >= 100000000 = 9
--  | v         >= 10000000  = 8
--  | v         >= 1000000   = 7
--  | v         >= 100000    = 6
--  | v         >= 10000     = 5
--  | v         >= 1000      = 4
--  | v         >= 100       = 3
--  | v         >= 10        = 2
--  | otherwise              = 1
--
--
--memcpy2 :: VM.Vector Char -> Int -> Int -> m ()
--memcpy2 v i i' = do
--  write v i $ digitTable M.! i'
--  write v (succ i) $ digitTable M.! succ i'
--
--digitTable :: V.Vector Char
--digitTable =
--  ['0','0','0','1','0','2','0','3','0','4','0','5','0','6','0','7','0','8','0','9',
--  ,'1','0','1','1','1','2','1','3','1','4','1','5','1','6','1','7','1','8','1','9',
--  ,'2','0','2','1','2','2','2','3','2','4','2','5','2','6','2','7','2','8','2','9',
--  ,'3','0','3','1','3','2','3','3','3','4','3','5','3','6','3','7','3','8','3','9',
--  ,'4','0','4','1','4','2','4','3','4','4','4','5','4','6','4','7','4','8','4','9',
--  ,'5','0','5','1','5','2','5','3','5','4','5','5','5','6','5','7','5','8','5','9',
--  ,'6','0','6','1','6','2','6','3','6','4','6','5','6','6','6','7','6','8','6','9',
--  ,'7','0','7','1','7','2','7','3','7','4','7','5','7','6','7','7','7','8','7','9',
--  ,'8','0','8','1','8','2','8','3','8','4','8','5','8','6','8','7','8','8','8','9',
--  ,'9','0','9','1','9','2','9','3','9','4','9','5','9','6','9','7','9','8','9','9']


d2s :: Double -> String
d2s = toWord >>> \w -> let
  sign = extractSign w
  ieeeExponent = extractExponent w
  ieeeMantissa = extractMantissa w
  in if ieeeExponent == maxExponent || (ieeeExponent == 0 && ieeeMantissa == 0)
    then specialCase sign ieeeExponent ieeeMantissa
    else let
      (mantissa, exponent) = fromMaybe (d2d ieeeMantissa ieeeExponent) $ d2dSmallInt ieeeMantissa ieeeExponent
      
d2d :: Mantissa a -> Exponent a -> (Mantissa a, Exponent a)
d2d ieeeMantissa ieeeExponent =
  if e2 >= 0
  
  else
  where
  mv = 4 * m2
  mmShift = ieeeMantissa != 0 || ieeeExponent <= 1

  --even = (m2 & 1) == 0
  acceptBounds = (m2 & 1) == 0

  (e2, m2) = case ieeeExponent of
    0 -> (findE2 1, ieeeMantissa)
    _ -> (ifindE2 eeeExponent, ieeeMantissaToM2 ieeeMantissa)
  findE2 = (- (bias + numMantissaBits + 2))

step3 e2
  | e2 >= 0 = let
    vr = mulShiftAll64 m2 (pow5InvSplit ! q) i
    e10 = q
    q = log10Pow2 e2 - e2 > 3
    k = pow5InvBitCount + pow5bits q - 1
    i = negate e2 + q + k
    in (vr, vp, vm, e10, vmIsTrailingZeros, vrIsTrailingZeros)
  | otherwise = let
    in (vr, vp, vm, e10, vmIsTrailingZeros, vrIsTrailingZeros)
  where

d2dSmallInt :: Mantissa a -> Exponent a -> Maybe (Mantissa a, Exponent a)
d2dSmallInt ieeeMantissa ieeeExponent = if e2 > 0 || e2 < maxE2ForFLessThanOne || fraction != 0
  then Nothing
  else Just (m2 >> e2N, 0)
  where
  m2 = ieeeMantissaToM2 ieeeMantissa
  e2 = ieeeExponent - bias
  e2N = negate e2
  mask = 1 `shiftL` e2N - 1
  fraction = m2 .&. mask

toWord :: a -> CompatableWord a
toWord = unsafeCoerce

ieeeMantissaToM2 :: MantissaWord a -> MantissaWord a
ieeeMantissa = (1 `shiftL` numMantissaBits .|.) 

--type CompatableWord :: Type -> Type
--type family CompatableWord a
--type instance CompatableWord Double = Word64

class FiniteBits a => BitCounts a where
  numExponentBits :: Int
  numMantissaBits :: Int
  numMantissaBits = finiteBitSize (0 :: a) - numExponentBits - 1

class SpecialValues a where
  positiveInfinity :: CompatableWord a
  negativeInfinity :: CompatableWord a
  positiveZero :: CompatableWord a
  negativeZero :: CompatableWord a
  isNan :: CompatableWord a -> Bool
  maxExponent :: CompatableWord a

class ExtractComponents a where
  extractSign :: CompatableWord a -> Sign
  extractExponent :: CompatableWord a -> Exponent a
  extractMantissa :: CompatableWord a -> Mantissa a

bias :: forall a. 

newtype Sign = Sign Bool

newtype Exponent a = Exponent {unExponent :: CompatableWord q}
type ExponentWord :: Type -> Type
type family ExponentWord a
type instance ExponentWord Double = Word32
type family ExponentInt a
type instance ExponentInt Double = Int32

newtype Mantissa a = Mantissa {unMantissa :: CompatableWord a}
type MantissaWord :: Type -> Type
type family MantissaWord a
type instance MantissaWord Double = Word64

class MaxE2ForFLessThanOne a where maxE2ForFLessThanOne :: ExponentInt a
instance MaxE2ForFLessThanOne Double where maxE2ForFLessThanOne = -52
