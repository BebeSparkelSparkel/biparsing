{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Biparse.Text.Numeric
  ( NaturalConstraints
  , naturalBaseTen
  , naturalBaseTen'
  , IntConstrints
  , intBaseTen
  , RealConstrints
  , realBaseTen
  ) where

import Biparse.Biparser (Iso, IsoClass(iso), split, upon, SubState, SubStateContext, try, ignoreBackward, split, Biparser, ElementContext, comap)
import Biparse.General (take)
import Biparse.Text (CharElement)
import Data.Char (isDigit)
import Data.Int (Int8, Int16, Int32, Int64)
import Data.Word (Word, Word8, Word16, Word32, Word64)
import GHC.Float (Float, Double)
import GHC.Num ((*))
import Safe (readMay)

instance NaturalConstraints c s m n Word   char => IsoClass c m n s Word   where iso = naturalBaseTen
instance NaturalConstraints c s m n Word8  char => IsoClass c m n s Word8  where iso = naturalBaseTen
instance NaturalConstraints c s m n Word16 char => IsoClass c m n s Word16 where iso = naturalBaseTen
instance NaturalConstraints c s m n Word32 char => IsoClass c m n s Word32 where iso = naturalBaseTen
instance NaturalConstraints c s m n Word64 char => IsoClass c m n s Word64 where iso = naturalBaseTen

type NaturalConstraints c s m n a char =
  ( CharElement c s char
  , Show a
  , IsSequence (SubState c s)
  , MonadState s m
  , MonadFail m
  , MonadWriter (SubState c s) n
  , MonadFail n
  , Read a
  , SubStateContext c s
  )

naturalBaseTen :: forall c s m n a char. NaturalConstraints c s m n a char => Iso c m n s a
naturalBaseTen = do
  ds <- fmap toChar . toList <$> digitsBaseTen
  maybe (fail $ "Could not parse " <> show ds <> " to natural base 10.") pure $ readMay ds

naturalBaseTen' :: forall a c s m n char. NaturalConstraints c s m n a char => Iso c m n s a
naturalBaseTen' = naturalBaseTen

instance NaturalConstraints c s m n Int   char => IsoClass c m n s Int   where iso = naturalBaseTen
instance NaturalConstraints c s m n Int8  char => IsoClass c m n s Int8  where iso = naturalBaseTen
instance NaturalConstraints c s m n Int16 char => IsoClass c m n s Int16 where iso = naturalBaseTen
instance NaturalConstraints c s m n Int32 char => IsoClass c m n s Int32 where iso = naturalBaseTen
instance NaturalConstraints c s m n Int64 char => IsoClass c m n s Int64 where iso = naturalBaseTen

type IntConstrints c s m n a char =
  ( NaturalConstraints c s m n a char
  , MonadPlus m
  , Alternative n
  , Num a
  , ElementContext c s
  )

intBaseTen :: forall c s m n a char. IntConstrints c s m n a char => Iso c m n s a
intBaseTen = do
  s <- sign
  n <- naturalBaseTen
  pure $ s * n

type RealConstrints c s m n a ss char =
  ( NaturalConstraints c s m n a char
  , MonadPlus m
  , ElementContext c s
  , Show ss
  , Alternative n
  , Num a
  , ss ~ SubState c s
  )

instance RealConstrints c s m n Float  ss char => IsoClass c m n s Float  where iso = realBaseTen
instance RealConstrints c s m n Double ss char => IsoClass c m n s Double where iso = realBaseTen

realBaseTen :: forall c s m n a ss char.
  ( RealConstrints c s m n a ss char
  ) => Iso c m n s a
realBaseTen = do
  s <- sign
  ws <- digitsBaseTen :: Biparser c s m n a ss
  ds <- comap (const mempty) $ ignoreBackward
    $ try (cons <$> (fromChar '.' <$ take (fromChar '.'))  <*> digitsBaseTen)
    <|> pure mempty
    :: Biparser c s m n a ss
  maybe (fail "Could not parse real base 10.") (pure . (* s)) $ readMay $ fmap toChar $ toList $ ws <> ds

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

sign :: forall c s m n a ss char.
  ( CharElement c s char
  , MonadState s m
  , MonadFail m
  , MonadPlus m
  , IsSequence ss
  , MonadWriter ss n
  , MonadFail n
  , Alternative n
  , Num a
  , ElementContext c s
  , ss ~ SubState c s
  ) => Biparser c s m n a a
sign = comap (const 1) $ ignoreBackward $ (-1) <$ try (take $ fromChar '-') <|> pure 1

