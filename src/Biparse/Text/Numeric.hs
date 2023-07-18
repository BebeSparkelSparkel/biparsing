{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Biparse.Text.Numeric
  ( NaturalConstraints
  , naturalBaseTen
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

instance NaturalConstraints c s m n Word   => IsoClass c m em n s Word   where iso = naturalBaseTen
instance NaturalConstraints c s m n Word8  => IsoClass c m em n s Word8  where iso = naturalBaseTen
instance NaturalConstraints c s m n Word16 => IsoClass c m em n s Word16 where iso = naturalBaseTen
instance NaturalConstraints c s m n Word32 => IsoClass c m em n s Word32 where iso = naturalBaseTen
instance NaturalConstraints c s m n Word64 => IsoClass c m em n s Word64 where iso = naturalBaseTen

type NaturalConstraints c s m n a =
  ( CharElement c s
  , Show a
  , IsSequence (SubState c s)
  , MonadState s m
  , MonadFail m
  , MonadWriter (SubState c s) n
  , MonadFail n
  , Read a
  , SubStateContext c s
  )

naturalBaseTen :: forall c s m em n a. NaturalConstraints c s m n a => Iso c m em n s a
naturalBaseTen = do
  ds <- toList <$> digitsBaseTen
  maybe (fail $ "Could not parse " <> show ds <> " to natural base 10.") pure $ readMay ds

instance NaturalConstraints c s m n Int   => IsoClass c m em n s Int   where iso = naturalBaseTen
instance NaturalConstraints c s m n Int8  => IsoClass c m em n s Int8  where iso = naturalBaseTen
instance NaturalConstraints c s m n Int16 => IsoClass c m em n s Int16 where iso = naturalBaseTen
instance NaturalConstraints c s m n Int32 => IsoClass c m em n s Int32 where iso = naturalBaseTen
instance NaturalConstraints c s m n Int64 => IsoClass c m em n s Int64 where iso = naturalBaseTen

type IntConstrints c s m n a =
  ( NaturalConstraints c s m n a
  , MonadPlus m
  , Alternative n
  , Num a
  , ElementContext c s
  )

intBaseTen :: forall c s m em n a. IntConstrints c s m n a => Iso c m em n s a
intBaseTen = do
  s <- sign
  n <- naturalBaseTen
  pure $ s * n

type RealConstrints c s m n a ss =
  ( NaturalConstraints c s m n a
  , MonadPlus m
  , ElementContext c s
  , Show ss
  , Alternative n
  , Num a
  , ss ~ SubState c s
  )

instance RealConstrints c s m n Float  ss => IsoClass c m em n s Float  where iso = realBaseTen
instance RealConstrints c s m n Double ss => IsoClass c m em n s Double where iso = realBaseTen

realBaseTen :: forall c s m em n a ss. RealConstrints c s m n a ss => Iso c m em n s a
realBaseTen = do
  s <- sign
  ws <- digitsBaseTen :: Biparser c s m em n a ss
  ds <- comap (const mempty) $ ignoreBackward
    $ try (cons <$> ('.' <$ take '.')  <*> digitsBaseTen)
    <|> pure mempty
    :: Biparser c s m em n a ss
  maybe (fail "Could not parse real base 10.") (pure . (* s)) $ readMay $ toList $ ws <> ds

digitsBaseTen :: forall c m em n s u ss.
  ( CharElement c s
  , Show u
  , IsSequence ss
  , MonadState s m
  , MonadWriter ss n
  , SubStateContext c s
  , ss ~ SubState c s
  ) => Biparser c s m em n u ss
digitsBaseTen = split (state $ span isDigit) `upon` fromList . show

sign :: forall c s m em n a ss.
  ( CharElement c s
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
  ) => Biparser c s m em n a a
sign = comap (const 1) $ ignoreBackward $ (-1) <$ try (take '-') <|> pure 1

