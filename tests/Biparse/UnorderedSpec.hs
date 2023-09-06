{-# LANGUAGE UndecidableInstances #-}
module Biparse.UnorderedSpec where

import Biparse.Unordered
import GHC.Generics (Generic)
import Biparse.Text.Context.LineColumn (startLineColumn)

spec :: Spec
spec = pure ()





-----------------------------------------------------
data ABC = ABC
  { def :: Int
  , ghi :: Accumulating [String]
  } deriving (Show, Generic)

added = unsafePerformIO $ addIORefs $ from $ ABC 1 (Accumulating ["abcdef"])
type IntStringList = [Either Int String]
parsers = makeParsers @IdentityState @(StateErrorT 'NewtypeInstance IntStringList IO) @(WriterT IntStringList IO) @IntStringList $ snd $ added

up = unorderdParser @IdentityState @(StateErrorT 'NewtypeInstance IntStringList IO) @(WriterT IntStringList IO) @IntStringList $ ABC 1 (Accumulating ["abcdef"])

instance Accumulate [a] where
  type AccumulateElement [a] = a
  accumulate = cons

instance
  ( MonadState IntStringList m
  , MonadFail m
  , Alternative m
  , MonadWriter IntStringList n
  , MonadFail n
  ) => IsoClass IdentityState m n IntStringList String where
  iso = comap Right $ one >>= \case
    Right x -> pure x
    _ -> fail $ "Expected right"

instance
  ( MonadState IntStringList m
  , MonadFail m
  , Alternative m
  , MonadWriter IntStringList n
  , MonadFail n
  ) => IsoClass IdentityState m n IntStringList Int where
  iso = comap Left $ one >>= \case
    Left x -> pure x
    _ -> fail $ "Expected left"

