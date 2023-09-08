{-# LANGUAGE UndecidableInstances #-}
module Biparse.UnorderedSpec where

import Biparse.Unordered
import GHC.Generics (Generic)
import Biparse.Text.Context.LineColumn (startLineColumn)
import Data.Tuple.Extra (uncurry3)
import Data.List (zip3, length)
import Data.Coerce
import Data.Default (Default(def))
import GHC.Num ((+))

spec :: Spec
spec = do
  fb @() "AllParserTypes"
    (unorderedBiparserDef :: Iso LinesOnly (FM Ts) EitherString (Position Ts) AllParserTypes)
    (\f -> do
      it "in order" do
        let result = AllParserTypes 1 (Accumulating ["Not Default"]) (Optional $ Just True)
        result `shouldNotBe` def
        singleSuccessParser result `shouldNotBe` singleSuccessParser def
        accumulatingParser result `shouldNotBe` accumulatingParser def
        optionalParser result `shouldNotBe` optionalParser def
        f [One 1, Two "Not Default", Three True] `shouldBe` Right (result, Position 4 1 mempty)

      prop "all given"
        \(i, s, b) -> forAll (shuffle [One i, Two s, Three b])
        \ts ->
        f (startLineColumn ts) `shouldBe` Right
          ( AllParserTypes i (Accumulating [s]) (Optional $ Just b)
          , Position 4 1 mempty
          )

      it "only required given" $
        f [One 5] `shouldBe` Right
          ( AllParserTypes 5 (Accumulating []) (Optional Nothing)
          , Position 2 1 mempty
          )

      prop "missing required" \(ss, b, ss') -> let
        l = case (length ss, length ss') of
          (0,0) -> 2
          (0,x) -> x + 2
          (x,0) -> x + 2
          (x,y) -> x + y + 2
        in f (startLineColumn $ (Two <$> ss) <> [Three b] <> (Two <$> ss')) `shouldSatisfy` errorPosition l 1

    )
    \b -> do
      prop "prints all"
        \apt@(AllParserTypes i (Accumulating ss) (Optional b')) ->
        b apt `shouldBe` EValue (apt, One i : (Two <$> ss) & maybe id (\x -> (`snoc` Three x)) b')


type T = TriSum Int String Bool
type Ts = [T]

data AllParserTypes = AllParserTypes
  { singleSuccessParser :: Int
  , accumulatingParser :: Accumulating [String]
  , optionalParser :: Optional Bool
  } deriving (Show, Eq, Generic)
instance Default AllParserTypes where
  def = AllParserTypes def (Accumulating def) (Optional def)
instance Arbitrary AllParserTypes where
  arbitrary = AllParserTypes <$> arbitrary <*> (Accumulating <$> arbitrary) <*> (Optional <$> arbitrary)
  shrink (AllParserTypes i (Accumulating ss) (Optional b)) = uncurry3 AllParserTypes . coerce <$> zip3 (shrink i) (shrink ss) (shrink b)

type IsoConstraints c m n a ss =
  ( T ~ SubElement c a
  , One c a m n ss
  , MonadFail n
  )

instance IsoConstraints c m n a ss => IsoClass c m n a Int where
  iso = comap One $ one >>= \case
    One x -> pure x
    _ -> fail $ "Expected One"

instance IsoConstraints c m n a ss => IsoClass c m n a String where
  iso = comap Two $ one >>= \case
    Two x -> pure x
    _ -> fail $ "Expected Two"

instance IsoConstraints c m n a ss => IsoClass c m n a Bool where
  iso = comap Three $ one >>= \case
    Three x -> pure x
    _ -> fail $ "Expected Three"
