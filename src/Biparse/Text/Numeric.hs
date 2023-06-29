{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Biparse.Text.Numeric
  ( naturalBaseTen
  , realBaseTen
  ) where

import Biparse.Biparser (Iso, IsoClass(iso), split, upon, SubState, SubStateContext, try, ignoreBackward, split, Biparser, ElementContext, comap)
import Biparse.General (take)
import Safe (readMay)
import Data.Char (isDigit)
import Biparse.Text (CharElement)
import Data.Word (Word, Word8, Word16, Word32, Word64)
import GHC.Num ((*))


instance NaturalConstraints c s m n Word => IsoClass c m n s Word where iso = naturalBaseTen
instance NaturalConstraints c s m n Word8 => IsoClass c m n s Word8 where iso = naturalBaseTen
instance NaturalConstraints c s m n Word16 => IsoClass c m n s Word16 where iso = naturalBaseTen
instance NaturalConstraints c s m n Word32 => IsoClass c m n s Word32 where iso = naturalBaseTen
instance NaturalConstraints c s m n Word64 => IsoClass c m n s Word64 where iso = naturalBaseTen

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

naturalBaseTen :: forall c s m n a. NaturalConstraints c s m n a => Iso c m n s a
naturalBaseTen = do
  ds <- digitsBaseTen
  maybe (fail "Could not parse natural base 10.") pure $ readMay $ toList ds

realBaseTen :: forall c s m n a ss.
  ( NaturalConstraints c s m n a
  , MonadPlus m
  , ElementContext c s
  , Show ss
  , Alternative n
  , Num a
  , ss ~ SubState c s
  ) => Iso c m n s a
realBaseTen = do
  sign :: a <- comap (const 1) $ ignoreBackward $ (-1) <$ try (take '-') <|> pure 1 :: Biparser c s m n a a
  ws <- digitsBaseTen :: Biparser c s m n a ss
  ds <- comap (const mempty) $ ignoreBackward
    $ try (cons <$> ('.' <$ take '.')  <*> digitsBaseTen)
    <|> pure mempty
    :: Biparser c s m n a ss
  maybe (fail "Could not parse real base 10.") (pure . (* sign)) $ readMay $ toList $ ws <> ds


digitsBaseTen :: forall c m n s u ss.
  ( CharElement c s
  , Show u
  , IsSequence ss
  , MonadState s m
  , MonadWriter ss n
  , SubStateContext c s
  , ss ~ SubState c s
  ) => Biparser c s m n u ss
digitsBaseTen = split (state $ span isDigit) `upon` fromList . show
