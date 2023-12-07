-- | Ryu: Fast Float-to-String Conversion
-- PLDI’18, June 18ś22, 2018, Philadelphia, PA,SA
-- C� 2018 Copyright held by the owner/author(s).
-- ACM ISBN 978-1-4503-5698-5/18/06.
-- https://doi.org/10.1145/3192366.3192369

{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}

{-# LANGUAGE PartialTypeSignatures #-}

module Biparse.Text.Numeric.Printers.Ryu
  ( ryu
  , ryuTotal
  , ryuNominals
  , NominalSubnominal
  , Notation(..)
  , ScientificNotation
  , Sign
  , Exponent(..)
  , Mantissa(..)
  , maskRight
  , max
  , NumBits(..)
  ) where

import Control.Monad.Extra (whenM)
import Control.Monad.Loops (whileM_)
import Control.Monad.ST.Safe (runST, ST)
import Data.Bits (Bits, shiftL, shiftR, rotateR, (.|.), (.&.))
import Data.Bool ((||))
import Data.Eq ((/=))
import Data.Int (Int32, Int64)
import Data.STRef (STRef, newSTRef, writeSTRef, modifySTRef, readSTRef)
import Data.Word (Word64, Word32)
import GHC.Enum (succ)
import GHC.Float (Double, Float)
import GHC.Num ((*), negate, subtract)
import GHC.Real (Real, Integral, fromIntegral, mod, div, (^))
import GHC.TypeNats (Nat)
import Numeric (showInt, showHex)
import Unsafe.Coerce (unsafeCoerce)

import Debug.Trace
import GHC.Err (undefined)

ryu :: forall notation acceptSmaller acceptLarger breakTieDown a text w i.
  ( Bits w
  , NumBits (Exponent a)
  , NumBits (Mantissa a)
  , Bias a
  , Integral w
  , Notation notation a text
  , w ~ CompatableWord a
  , i ~ CompatableInt a
  , Show i
  , Integral i
  , BoolResolve acceptSmaller
  , BoolResolve acceptLarger
  , BoolResolve breakTieDown
  , IsString text
  , Integral w
  , Default (NonNormalReturns notation text)
  , Show w
  , a ~ Double
  ) => a -> text
ryu = ryuTotal @notation @acceptSmaller @acceptLarger @breakTieDown def

ryuTotal :: forall notation acceptSmaller acceptLarger breakTieDown a text w i.
  ( Bits w
  , NumBits (Exponent a)
  , NumBits (Mantissa a)
  , Bias a
  , Integral w
  , Notation notation a text
  , w ~ CompatableWord a
  , i ~ CompatableInt a
  , Show i
  , Integral i
  , BoolResolve acceptSmaller
  , BoolResolve acceptLarger
  , Integral w
  , Show w
  , a ~ Double
  , BoolResolve breakTieDown
  ) => NonNormalReturns notation text -> a -> text
ryuTotal (NonNormalReturns {..}) f
  | w == 0 = positiveZero
  | w == negZero = negativeZero
  | w == posInf @a = positiveInfinity
  | w == negInf @a = negativeInfinity
  | isNaN ieeeExponent ieeeMantissa = nan
  | otherwise = ryuNominalSubnominal @notation @acceptSmaller @acceptLarger @breakTieDown ieeeSign ieeeExponent ieeeMantissa
  where
  ieeeSign     = extractSign     f
  ieeeExponent = extractExponent f
  ieeeMantissa = extractMantissa f
  w = toWord f

data NonNormalReturns notation text = NonNormalReturns
  { negativeInfinity :: text
  , positiveInfinity :: text
  , nan              :: text
  , negativeZero     :: text
  , positiveZero     :: text
  } deriving Show
instance IsString text => Default (NonNormalReturns ScientificNotation text) where
  def = NonNormalReturns
    { negativeInfinity = "-Infinity"
    , positiveInfinity = "Infinity"
    , nan              = "NaN"
    , negativeZero     = "-0E0"
    , positiveZero     = "0E0"
    }

ryuNominals :: forall notation acceptSmaller acceptLarger breakTieDown a m text w i.
  ( MonadFail m
  , Bits w
  , NumBits (Exponent a)
  , NumBits (Mantissa a)
  , Bias a
  , Integral w
  , Show a
  , Notation notation a text
  , w ~ CompatableWord a
  , i ~ CompatableInt a
  , Show i
  , Integral i
  , BoolResolve acceptSmaller
  , BoolResolve acceptLarger
  , BoolResolve breakTieDown
  , Show w
  , a ~ Double
  ) => NominalSubnominal a -> m text
ryuNominals (NominalSubnominal f)
  = when (isInfOrNaN ieeeExponent ieeeMantissa) (fail $ "Expected a nominal or subnominal number but received: " <> show f)
  $> ryuNominalSubnominal @notation @acceptSmaller @acceptLarger @breakTieDown ieeeSign ieeeExponent ieeeMantissa
  where
  ieeeSign = extractSign f
  ieeeExponent = extractExponent f
  ieeeMantissa = extractMantissa f

isInfOrNaN ::
  ( NumBits (Exponent a)
  , Num w
  , Bits w
  , w ~ CompatableWord a
  ) => Exponent a -> Mantissa a -> Bool
isInfOrNaN ieeeExponent ieeeMantissa
  = ieeeExponent == max
  || (  ieeeExponent == 0
     && ieeeMantissa /= 0 )

--isZero :: forall a.
--  ( Bits a
--  , Num a
--  ) => a -> Bool
--isZero = (== 0) . (.|. negZero)

isNaN :: (Integral w, Num w, Bits w, NumBits (Exponent a), w ~ CompatableWord a) => Exponent a -> Mantissa a -> Bool
isNaN e m = e == max && m /= 0

newtype NominalSubnominal f = NominalSubnominal f

negZero :: (Bits a, Num a) => a
negZero = 1 `rotateR` 1

posInf :: forall a w. (NumBits (Mantissa a), NumBits (Exponent a), Bits w, Num w, w ~ CompatableWord a) => w
posInf = unExponent @a $ maskRight `shiftL` numBits @(Mantissa a)

negInf :: forall a w. (NumBits (Mantissa a), NumBits (Exponent a), Bits w, Num w, w ~ CompatableWord a) => w
negInf = negZero .|. posInf @a

type Sign = Bool
extractSign :: forall a w. (Bits w, Num w, w ~ CompatableWord a) => a -> Sign
extractSign = (/= 0) . (negZero .&.) . toWord

extractExponent :: forall a w. (NumBits (Mantissa a), NumBits (Exponent a), Num w, Bits w, w ~ CompatableWord a) => a -> Exponent a
extractExponent = (.&. maskRight) . Exponent . (`shiftR` numBits @(Mantissa a)) . toWord

extractMantissa :: forall a w. (NumBits (Mantissa a), Bits w, Num w, w ~ CompatableWord a) => a -> Mantissa a
extractMantissa = (.&. maskRight) . Mantissa . toWord

toWord :: a -> CompatableWord a
toWord = unsafeCoerce

--type MutableStringType :: Type -> Type
--type family MutableStringType text
--type instance MutableStringType text = VM.Vector (Element text)

type CompatableWord :: Type -> Type
type CompatableWord a = BitsToWord (NumberOfBits a)
type BitsToWord :: Nat -> Type
type family BitsToWord a where
  BitsToWord 32 = Word32
  BitsToWord 64 = Word64

type CompatableInt :: Type -> Type
type CompatableInt a = BitsToInt (NumberOfBits a)
type BitsToInt :: Nat -> Type
type family BitsToInt a where
  BitsToInt 32 = Int32
  BitsToInt 64 = Int64

type NumberOfBits :: Type -> Nat
type family NumberOfBits a
type instance NumberOfBits Float = 32
type instance NumberOfBits Double = 64

newtype Mantissa a = Mantissa (CompatableWord a)
instance Integral (CompatableWord a) => Show (Mantissa a) where show = ("Mantissa " <>) . ($ mempty) . showHex
deriving instance Bits     (CompatableWord a) => Bits     (Mantissa a)
deriving instance Num      (CompatableWord a) => Num      (Mantissa a)
deriving instance Eq       (CompatableWord a) => Eq       (Mantissa a)
deriving instance Ord      (CompatableWord a) => Ord      (Mantissa a)
deriving instance Real     (CompatableWord a) => Real     (Mantissa a)
deriving instance Integral (CompatableWord a) => Integral (Mantissa a)
deriving instance Enum     (CompatableWord a) => Enum     (Mantissa a)

newtype Exponent a = Exponent {unExponent :: CompatableWord a}
instance Integral (CompatableWord a) => Show (Exponent a) where show = ("Exponent " <>) . ($ mempty) . showHex
deriving instance Bits     (CompatableWord a) => Bits     (Exponent a)
deriving instance Num      (CompatableWord a) => Num      (Exponent a)
deriving instance Eq       (CompatableWord a) => Eq       (Exponent a)
deriving instance Ord      (CompatableWord a) => Ord      (Exponent a)
deriving instance Real     (CompatableWord a) => Real     (Exponent a)
deriving instance Integral (CompatableWord a) => Integral (Exponent a)
deriving instance Enum     (CompatableWord a) => Enum     (Exponent a)

newtype DecimalRepresentation a = DecimalRepresentation (CompatableWord a)
deriving instance Show (CompatableWord a) => Show (DecimalRepresentation a)

maskRight :: forall a. (NumBits a, Num a, Bits a) => a
maskRight = 1 `shiftL` numBits @a - 1

max :: forall a. (NumBits a, Num a, Bits a) => a
max = maskRight

class NumBits a where numBits :: Int
instance NumBits (Exponent Double) where numBits = 11
instance NumBits (Mantissa Double) where numBits = 52

ryuNominalSubnominal :: forall notation acceptSmaller acceptLarger breakTieDown a text w i.
  ( NumBits (Mantissa a)
  , Bias a
  , Num (Exponent a)
  , Bits w
  , Integral w
  , Num i
  , Show i
  , Ord i
  , Integral i
  , Notation notation a text
  , w ~ CompatableWord a
  , i ~ CompatableInt a
  , BoolResolve acceptSmaller
  , BoolResolve acceptLarger
  , BoolResolve breakTieDown
  , Show w
  , a ~ Double
  ) => Sign -> Exponent a -> Mantissa a -> text
ryuNominalSubnominal s e m = result
  where
  -- Step 5. Print the decimal representation.
  result = notation @notation s d0 e0
  -- Step 4. Find a shortest, correctly-rounded decimal representation in the interval of legal representations.
  (d0,e0) = traceShowId $ computeShortest24 @a a b c (boolResolve @acceptSmaller) (boolResolve @acceptLarger) (boolResolve @breakTieDown)
  -- Step 3. Convert (u,v,w) ·  ^ 2e2 to a decimal power bae
  (_ {- e10 -}, a, b, c) = if e2 >= 0
    then let x = 1 `shiftL'` e2 in (0,  u * x, v * x, w * x)
    else let x = 5 ^ negate e2 in (e2, u * x, v * x, w * x)
  -- Step 2. Determine the interval of information-preserving outputs.
  e2 = ef - 2 :: CompatableInt a
  u = v - if m == 0 && e > 1 then 1 else 2 :: w
  v = coerce $ 4 * mf :: w
  w = v + 2 :: w
  -- Step 1. Decode the floating point number, and unify normalized and subnormal cases.
  (mf,ef) = if e /= 0
    then ( 1 `shiftL` lenMantissa .|. m -- 2 ^ len m + m
         , unsafeCoerce e - bl )
    else ( m
         , 1 - bl )
  bl = fromIntegral $ bias @a + lenMantissa
  lenMantissa = numBits @(Mantissa a)

--data ShortestSettings = ShortestSettings
--  { acceptSmaller :: Bool
--  , acceptLarger :: Bool
--  , breakTieDown :: Bool
--  }
--type family AcceptSmaller a where AcceptSmaller ('ShortestSettings acceptSmaller _            _            ) = acceptSmaller
--type family AcceptLarger  a where AcceptLarger  ('ShortestSettings _             acceptLarger _            ) = acceptLarger
--type family BreakTieDown  a where BreakTieDown  ('ShortestSettings _             _            breakTieDown ) = breakTieDown

type BoolResolve :: Bool -> Constraint
class BoolResolve (a :: Bool) where boolResolve :: Bool
instance BoolResolve 'True  where boolResolve = True
instance BoolResolve 'False where boolResolve = False

class Bias a where bias :: Int
instance Bias Double where bias = 1023

computeShortest24 :: forall a w.
  ( Integral w
  , LiftST (ClassInstance w)
  , w ~ SubType w
  , w ~ CompatableWord a
  , a ~ Double
  ) => w -> w -> w -> Bool -> Bool -> Bool -> (DecimalRepresentation a, Exponent a)
computeShortest24 a' b' c' acceptSmaller acceptLarger breakTieDown = runST do
  digit <- newSTRef 0
  all_a_zero <- newSTRef True
  all_b_zero <- newSTRef True
  a <- newSTRef a'
  b <- newSTRef b'
  c <- newSTRef $ bool pred id acceptLarger c'
  i <- newSTRef 0
  let loopCommon = do
        all_b_zero &&=^ digit ^==^ zero
        digit =^ b ^%^ ten
        a //= 10
        b //= 10
        c //= 10
        inc i
  whileM_ (a ^/^ ten ^<^ c ^/^ ten) do
    all_a_zero &&=^ a ^%^ ten ^==^ zero
    loopCommon
  whenM (acceptSmaller ^&&^ all_a_zero) $
    whileM_ (a ^%^ ten ^==^ zero) loopCommon
  let isTie = digit ^==^ five ^&&^ all_b_zero
      wantRoundDown = digit ^<^ five ^||^ (isTie ^&&^ breakTieDown)
      roundDown = (wantRoundDown ^&&^ (a ^!=^ b ^||^ all_a_zero)) ^||^ b ^+^ one ^>^ c
  liftA2 (,) (coerce $ ifM roundDown (readSTRef b) (b ^+^ one)) (readSTRef i)
  where
  zero = 0 :: w
  one = 1 :: w
  five = 5 :: w
  ten = 10 :: w

data CI = STMonad | STReference | Value

type LiftST :: CI -> Constraint
class LiftST m where liftST :: (a -> b) -> InjectS m s a -> ST s b
instance LiftST STMonad where liftST = fmap
instance LiftST STReference where liftST f = fmap f . readSTRef
instance LiftST Value where liftST f = pure . f

type InjectS :: CI -> Type -> Type -> Type
type  family InjectS m s a where
  InjectS STReference s a = STRef s a
  InjectS STMonad s a = ST s a
  InjectS Value _ a = a

type ClassInstance :: Type -> CI
type  family ClassInstance a where
  ClassInstance (STRef _ _) = STReference
  ClassInstance (ST _ _) = STMonad
  ClassInstance _ = Value

type SubType :: Type -> Type
type family SubType a where
  SubType (STRef _ a) = a
  SubType (ST _ a) = a
  SubType a = a

id' :: forall (r :: Type) (a :: Type) (s :: Type) (ci :: CI).
  ( LiftST ci
  , r ~ InjectS ci s a
  , ci ~ ClassInstance r
  ) => r -> ST s a
id' = liftST @ci id

testOne :: Bool
testOne = runST f
  where
  f :: forall s. ST s Bool
  f = do
    a <- id' =<< newSTRef True
    b <- id' (pure False :: ST s Bool)
    c <- id' True
    pure $ a && b && c

type LiftST2 (cim :: CI) (cin :: CI) a b m n c s =
  ( LiftST cim
  , LiftST cin
  , a ~ SubType m
  , b ~ SubType n
  , m ~ InjectS cim s a
  , n ~ InjectS cin s b
  , cim ~ ClassInstance m
  , cin ~ ClassInstance n
  )
liftST2 :: forall (cim :: CI) (cin :: CI) a b m n c s.
  LiftST2 cim cin a b m n c s
  => (a -> b -> c) -> m -> n -> ST s c
liftST2 f x y = liftST @cim f x >>= \g -> liftST @cin g y

testTwo :: Int
testTwo = runST do
  all_b_zero <- newSTRef True
  digit <- newSTRef 0
  all_b_zero &&=^ digit ^==^ (0 :: Int)
  readSTRef digit

testThree :: Bool
testThree = runST do
  x <- newSTRef 1
  let y = 2 :: Int
  x ^==^ y

infix 4 ^<^
(^<^) :: forall m n s a cim cin.
  ( Ord a
  , LiftST2 cim cin a a m n Bool s
  ) => m -> n -> ST s Bool
(^<^) = liftST2 (<)
infix 4 ^>^
(^>^) :: forall m n s a cim cin.
  ( Ord a
  , LiftST2 cim cin a a m n Bool s
  ) => m -> n -> ST s Bool
(^>^) = liftST2 (>)
infix 7 ^/^
(^/^) :: forall m n s a cim cin.
  ( Integral a
  , LiftST2 cim cin a a m n a s
  ) => m -> n -> ST s a
(^/^) = liftST2 div
infix 6 ^+^
(^+^) :: forall m n s a cim cin.
  ( Num a
  , LiftST2 cim cin a a m n a s
  ) => m -> n -> ST s a
(^+^) = liftST2 (+)
infix 7 ^%^
(^%^) :: forall m n s a cim cin.
  ( Integral a
  , LiftST2 cim cin a a m n a s
  ) => m -> n -> ST s a
(^%^) = liftST2 mod
infix 4 ^==^
(^==^) :: forall m n s a cim cin.
  ( Eq a
  , LiftST2 cim cin a a m n Bool s
  ) => m -> n -> ST s Bool
(^==^) = liftST2 (==)
infix 4 ^!=^
(^!=^) :: forall m n s a cim cin.
  ( Eq a
  , LiftST2 cim cin a a m n Bool s
  ) => m -> n -> ST s Bool
(^!=^) = liftST2 (/=)
infixr 3 ^&&^
(^&&^) :: LiftST2 cim cin Bool Bool m n Bool s => m -> n -> ST s Bool
(^&&^) = liftST2 (&&)
infixr 3 ^||^
(^||^) :: LiftST2 cim cin Bool Bool m n Bool s => m -> n -> ST s Bool
(^||^) = liftST2 (||)


--infix 4 ^<^
--(^<^) :: (Ord a, ReadST m, ReadST n) => m s a -> n s a -> ST s Bool
--(^<^) = bothLifted (<)
--infix 4 ^<
--(^<) :: (Ord a, ReadST m) => m s a -> a -> ST s Bool
--(^<) = leftLifted (<)
--infix 4 ^>^
--(^>^) :: (Ord a, ReadST m, ReadST n) => m s a -> n s a -> ST s Bool
--(^>^) = bothLifted (>)
--infix 7 ^/
--(^/) :: (ReadST m, Integral a) => m s a -> a -> ST s a
--(^/) = leftLifted div
--infix 6 ^+
--(^+) :: (Num a, ReadST m) => m s a -> a -> ST s a
--(^+) = leftLifted (+)
--infix 7 ^%
--(^%) :: (ReadST m, Integral a) => m s a -> a -> ST s a
--(^%) = leftLifted mod
--infix 4 ^==
--(^==) :: (Eq a, ReadST m) => m s a -> a -> ST s Bool
--(^==) = leftLifted (==)
--infix 4 ^!=^
--(^!=^) :: (Eq a, ReadST m, ReadST n) => m s a -> n s a -> ST s Bool
--(^!=^) = bothLifted (/=)
--infixr 3 ^&&^
--(^&&^) :: (ReadST m, ReadST n) => m s Bool -> n s Bool -> ST s Bool
--(^&&^) = bothLifted (&&)
----infixr 3 &&^
----(&&^) :: ReadST m => Bool -> m s Bool -> ST s Bool
----x &&^ y = (x &&) <$> read y
--infixr 3 ^||^
--(^||^) :: (ReadST m, ReadST n) => m s Bool -> n s Bool -> ST s Bool
--(^||^) = bothLifted (||)
----infixr 3 ^||
----(^||) :: ReadST m => m s Bool -> Bool -> ST s Bool
----x ^|| y = liftA2 (||) (read x) (read y)
inc :: Enum a => STRef s a -> ST s ()
inc = (`modifySTRef` succ)
infix 2 //=
(//=) :: Integral a => STRef s a -> a -> ST s ()
x //= y = modifySTRef x (`div` y)
infix 2 &&=^
(&&=^) :: STRef s Bool -> ST s Bool -> ST s ()
x &&=^ y = modifySTRef x . (&&) =<< y
infix 2 =^
(=^) :: STRef s a -> ST s a -> ST s ()
x =^ y = writeSTRef x =<< read y
class ReadST m where read :: m s a -> ST s a
instance ReadST STRef where read = readSTRef
instance ReadST ST where read = id
bothLifted :: (ReadST m, ReadST n) => (a -> a -> b) -> m s a -> n s a -> ST s b
bothLifted f x y = liftA2 f (read x) (read y)
leftLifted :: ReadST m => (a -> a -> b) -> m s a -> a -> ST s b
leftLifted f x y = read x <&> (`f` y)

--computeShortest24 :: forall a w.
--  ( Integral w
--  , w ~ CompatableWord a
--  ) => w -> w -> w -> Bool -> Bool -> Bool -> (DecimalRepresentation a, Exponent a)
--computeShortest24 a b c acceptSmaller acceptLarger breakTieDown
--  = l0 a b (c & if acceptLarger then id else (subtract 1)) 0 0 True True
--  & (\(l1Args, all_a_zero) -> (if acceptSmaller && all_a_zero then uncurry6 l1 l1Args else l1Args)
--    & (\(a', b', c', i, digit, all_b_zero) -> let
--      isTie = digit == 5 && all_b_zero
--      wantRoundDown = digit < 5 || (isTie && breakTieDown)
--      roundDown = (wantRoundDown && (a' /= b' || all_a_zero)) || b' + 1 > c'
--      in (DecimalRepresentation if roundDown then b' else b' + 1, Exponent i)
--    )
--  )
--  where
--  l0 a' b' c' i digit all_a_zero all_b_zero = if a'' < c''
--    then let b'' = d10 b'
--             digit' = m10 b'
--             all_a_zero' = all_a_zero && m10 a' == 0
--             all_b_zero' = all_b_zero && digit == 0
--             in l0 a'' b'' c'' (succ i) digit' all_a_zero' all_b_zero'
--    else ((a', b', c', i, digit, all_b_zero), all_a_zero)
--    where
--    a'' = d10 a'
--    c'' = d10 c'
--  l1 a' b' c' i digit all_b_zero = if m10 a' == 0
--    then let a'' = d10 a'
--             b'' = d10 b'
--             c'' = d10 c'
--             digit' = m10 b'
--             all_b_zero' = all_b_zero && digit == 0
--             in l1 a'' b'' c'' (succ i) digit' all_b_zero'
--    else (a', b', c', i, digit, all_b_zero)
--  d10 = (`div` 10) :: w -> w
--  m10 = (`mod` 10) :: w -> w

--computeShortest23 a _ c acceptSmaller acceptLarger _
--  = l0 a (c & if acceptLarger then id else (- 1)) 0 True
--  & \(aci@(_,ci), all_a_zero) -> if acceptSmaller && all_a_zero
--    then uncurry3 l1 aci
--    else ci
--  where
--  l0 a' c' i all_a_zero = if a'' < c''
--    then l0 a'' c'' (succ i) (all_a_zero && a' `mod` 10 == 0)
--    else ((a', (c', i)), all_a_zero)
--    where
--    a'' = a' `div` 10
--    c'' = c' `div` 10
--  l1 a' c' i = if a' `mod` 10 == 0
--    then l1 (a' `div` 10) (c' `div` 10) (succ i)
--    else (c', i)

uncurry6 :: (a -> b -> c -> d -> e -> f -> g) -> (a, b, c, d, e, f) -> g
uncurry6 f (x0,x1,x2,x3,x4,x5) = f x0 x1 x2 x3 x4 x5

data ScientificNotation

class Notation notation a text where notation :: Sign -> DecimalRepresentation a -> Exponent a -> text
instance Notation ScientificNotation Double String where
  notation sign (DecimalRepresentation dr) (Exponent e)
    = bool id ('-' :) sign $ case showInt dr $ 'E' : show e of
      xs@(x : 'E' : _) -> xs
      x : xs -> x : '.' : xs

infixl 8 `shiftL'`
shiftL' :: (Bits a, Integral b) => a -> b -> a
shiftL' x y = shiftL x $ fromIntegral y

