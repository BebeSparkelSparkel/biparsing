{-# LANGUAGE UndecidableInstances #-}
module Biparse.UnorderedSpec where

import Biparse.Unordered
import GHC.Generics (Generic)
import Biparse.Text.Context.LineColumn (startLineColumn)

spec :: Spec
spec = focus do
  fb @() "ISB"
    (unordered :: Iso LinesOnly (FM Ts) EitherString (Position Ts) ISB)
    (\f -> do
      describe "success" do
        prop "success" \(i,s,b,t) -> forAll (shuffle [One i, Two s, Three b]) \h ->
          f (startLineColumn (h <> t)) `shouldBe` Right (ISB i s b, Position 4 1 t)

      describe "fail" do
        xit "Only Two" $ f [Two "False", Two "True"] `shouldSatisfy` errorPosition 2 1
        xit "Only One" $ f [One 0, One 1] `shouldSatisfy` errorPosition 1 1
    )
    \b -> do
      prop "print all" \v@(ISB i s b') ->
        b v `shouldBe` EValue (v, [One i, Two s, Three b'])

type Ts = [TriSum Int String Bool]

data TriSum a b c = One a | Two b | Three c deriving (Show, Eq)

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (TriSum a b c) where
  arbitrary = oneof [One <$> arbitrary, Two <$> arbitrary, Three <$> arbitrary]
  shrink = \case
    One x -> One <$> shrink x
    Two x -> Two <$> shrink x
    Three x -> Three <$> shrink x

oneWU :: WrapUnwrap (TriSum a b c) a
oneWU = WrapUnwrap (\case One x -> pure x; _ -> empty;) (pure . One)

twoWU :: WrapUnwrap (TriSum a b c) b
twoWU = WrapUnwrap (\case Two x -> pure x; _ -> empty;) (pure . Two)

threeWU :: WrapUnwrap (TriSum a b c) c
threeWU = WrapUnwrap (\case Three x -> pure x; _ -> empty;) (pure. Three)

data ISB = ISB Int String Bool deriving (Show, Eq, Generic)

instance Arbitrary ISB where
  arbitrary = ISB <$> arbitrary <*> arbitrary <*> arbitrary
  shrink (ISB i s b) = shrink (i,s,b) <&> \(x,y,z) -> ISB x y z

instance (MonadPlus m, Alternative n, One c s m n Ts) => IsoClass c m n s String where
  iso = wrappedOne twoWU

instance (MonadPlus m, Alternative n, One c s m n Ts) => IsoClass c m n s Int where
  iso = wrappedOne oneWU

instance (MonadPlus m, Alternative n, One c s m n Ts) => IsoClass c m n s Bool where
  iso = wrappedOne threeWU

