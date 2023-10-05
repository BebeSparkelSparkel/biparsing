{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Biparse.Text.Numeric
  ( NaturalConstraints
  , naturalBaseTen
  , naturalBaseTen'
  , IntConstrints
  , intBaseTen
  , scientific
  , RealConstrints
  , realBaseTen
  ) where

import Biparse.Biparser (Iso, IsoClass(iso), split, upon, SubState, SubStateContext, try, ignoreBackward, split, Biparser, ElementContext, comap)
import Biparse.General (take, optional, takeTri)
import Biparse.Text (CharElement)
import Data.Char (isDigit)
import Data.Int (Int8, Int16, Int32, Int64)
import Data.Word (Word, Word8, Word16, Word32, Word64)
import GHC.Float (Float, Double)
import GHC.Num ((*), negate, Integer, abs)
import GHC.Real (Fractional, (^^))
import Safe (readMay)

instance NaturalConstraints c s m n Word   char => IsoClass c m n s Word   where iso = naturalBaseTen
instance NaturalConstraints c s m n Word8  char => IsoClass c m n s Word8  where iso = naturalBaseTen
instance NaturalConstraints c s m n Word16 char => IsoClass c m n s Word16 where iso = naturalBaseTen
instance NaturalConstraints c s m n Word32 char => IsoClass c m n s Word32 where iso = naturalBaseTen
instance NaturalConstraints c s m n Word64 char => IsoClass c m n s Word64 where iso = naturalBaseTen

type NaturalConstraints c s m n number char =
  ( CharElement c s char
  , IsSequence (SubState c s)
  , MonadState s m
  , MonadFail m
  , MonadWriter (SubState c s) n
  , MonadFail n
  , SubStateContext c s
  , Read number
  , Num number
  , Show number
  )

naturalBaseTen :: forall c s m n number char. NaturalConstraints c s m n number char => Iso c m n s number
naturalBaseTen = do
  ds <- fmap toChar . toList <$> digitsBaseTen `upon` abs
  maybe (fail $ "Could not parse " <> show ds <> " to natural base 10.") pure $ readMay ds

naturalBaseTen' :: forall number c s m n char. NaturalConstraints c s m n number char => Iso c m n s number
naturalBaseTen' = naturalBaseTen

instance NaturalConstraints c s m n Int   char => IsoClass c m n s Int   where iso = naturalBaseTen
instance NaturalConstraints c s m n Int8  char => IsoClass c m n s Int8  where iso = naturalBaseTen
instance NaturalConstraints c s m n Int16 char => IsoClass c m n s Int16 where iso = naturalBaseTen
instance NaturalConstraints c s m n Int32 char => IsoClass c m n s Int32 where iso = naturalBaseTen
instance NaturalConstraints c s m n Int64 char => IsoClass c m n s Int64 where iso = naturalBaseTen

type IntConstrints c s m n number char =
  ( NaturalConstraints c s m n number char
  , MonadPlus m
  , Alternative n
  , Num number
  , Ord number
  , ElementContext c s
  )

intBaseTen :: forall c s m n number char. IntConstrints c s m n number char => Iso c m n s number
intBaseTen = do
  s <- sign
  n <- naturalBaseTen
  pure $ s n

type RealConstrints c s m n number ss char =
  ( NaturalConstraints c s m n number char
  , MonadPlus m
  , ElementContext c s
  , Show ss
  , Alternative n
  , Num number
  , Ord number
  , ss ~ SubState c s
  )

instance RealConstrints c s m n Float  ss char => IsoClass c m n s Float  where iso = realBaseTen
instance RealConstrints c s m n Double ss char => IsoClass c m n s Double where iso = realBaseTen

-- | Only wirtes digits and not powers of 10.
scientific :: forall c s m n number ss char.
  ( Fractional number
  , RealConstrints c s m n number ss char
  ) => Iso c m n s number
scientific = do
  digits <- realBaseTen
  power :: Maybe Integer <- comap (const Nothing) $ optional do
    take (fromChar 'E') <|> take (fromChar 'e')
    intBaseTen
  pure $ maybe id ((*) . (10 ^^)) power $ digits

realBaseTen :: forall c s m n number ss char.
  ( RealConstrints c s m n number ss char
  ) => Iso c m n s number
realBaseTen = do
  s <- sign
  ws <- digitsBaseTen `upon` abs
  ds <- comap (const mempty) $ ignoreBackward
    $   try (cons <$> (fromChar '.' <$ take (fromChar '.'))  <*> digitsBaseTen)
    <|> pure mempty
  maybe (fail "Could not parse real base 10.") (pure . s) $ readMay $ fmap toChar $ toList $ ws <> ds

digitsBaseTen :: forall c m n s u ss char.
  ( CharElement c s char
  , Show u
  , IsSequence ss
  , MonadState s m
  , MonadWriter ss n
  , SubStateContext c s
  , ss ~ SubState c s
  ) => Biparser c s m n u ss
digitsBaseTen = split (state $ span $ isDigit . toChar) `upon` fromList . fmap fromChar . show

data Sign = Positive | Negative | Zero deriving (Show, Eq)
deduceSign :: forall number.
  ( Ord number
  , Num number
  )
  => number
  -> Sign
deduceSign x
  | x < 0 = Negative
  | x > 0 = Positive
  | otherwise = Zero

sign :: forall c s m n number ss char.
  ( CharElement c s char
  , MonadState s m
  , MonadFail m
  , MonadPlus m
  , IsSequence ss
  , MonadWriter ss n
  , MonadFail n
  , Alternative n
  , Num number
  , Ord number
  , ElementContext c s
  , ss ~ SubState c s
  ) => Biparser c s m n number (number -> number)
sign = comap deduceSign $ takeTri (fromChar '-') Negative negate <|> pure id
--sign = comap (const 1) $ ignoreBackward $ (-1) <$ try (take $ fromChar '-') <|> pure 1

