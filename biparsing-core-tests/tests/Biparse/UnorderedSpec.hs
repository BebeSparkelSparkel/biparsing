{-# LANGUAGE UndecidableInstances #-}
module Biparse.UnorderedSpec where

import Biparse.Unordered

spec :: Spec
spec = do
  fb "AllParserTypes"
    (unorderedDef :: Iso LinesOnly (FM Ts) EitherString () Ts () (Position () Ts) AllParserTypes)
    ()
    ()
    (\f -> do
      it "in order" do
        let result = AllParserTypes 1 (Accumulating ["Not Default"]) (Optional $ Just True)
        accumulatingParser result `shouldNotBe` accumulatingParser def
        optionalParser result `shouldNotBe` optionalParser def
        f [One 1, Two "Not Default", Three True] `shouldBe` Right (result, Position () 4 1 mempty)

      prop "all given"
        \(i, s, b) -> forAll (shuffle [One i, Two s, Three b])
        \ts ->
        f (startLineColumn ts) `shouldBe` Right
          ( AllParserTypes i (Accumulating [s]) (Optional $ Just b)
          , Position () 4 1 mempty
          )

      it "only required given" $
        f [One 5] `shouldBe` Right
          ( AllParserTypes 5 (Accumulating []) (Optional Nothing)
          , Position () 2 1 mempty
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

data TriSum a b c = One a | Two b | Three c deriving (Show, Eq)
instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (TriSum a b c) where
  arbitrary = oneof [One <$> arbitrary, Two <$> arbitrary, Three <$> arbitrary]
  shrink = \case
    One x -> One <$> shrink x
    Two x -> Two <$> shrink x
    Three x -> Three <$> shrink x

data AllParserTypes = AllParserTypes
  { singleSuccessParser :: Int
  , accumulatingParser :: Accumulating [String]
  , optionalParser :: Optional Bool
  } deriving (Show, Eq, Generic)
instance Default AllParserTypes where
  def = AllParserTypes undefined (Accumulating def) (Optional def)
instance Arbitrary AllParserTypes where
  arbitrary = AllParserTypes <$> arbitrary <*> (Accumulating <$> arbitrary) <*> (Optional <$> arbitrary)
  shrink (AllParserTypes i (Accumulating ss) (Optional b))
    =   (\(x,(y,z)) -> AllParserTypes x y z) . coerce
    <$> (zip (shrink i) $ zip (shrink ss) (shrink b))

type IsoConstraints c m n a ss =
  ( T ~ SubElement a
  , One c a m n ss T ss
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
