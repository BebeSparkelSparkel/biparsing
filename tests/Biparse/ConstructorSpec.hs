{-# LANGUAGE UndecidableInstances #-}
module Biparse.ConstructorSpec where

import Biparse.Constructor
import GHC.Num ((+))
import Control.Lens (_1, _2, (^.))
import Control.Monad.ChangeMonad (Lift)

spec :: Spec
spec = do
  describe "focusOne" do
    fb @() "one"
      (focusOne @Lift @EitherString headAlt do
        expectFwd _1 1
        x <- exposes (^. _2)
        return $ x + 1
      :: Biparser IdentityState [(Int,Double)] EitherString EitherString [(Int,Double)] Double)
      (\f -> do
        it "empty" $ f empty `shouldSatisfy` isString

        it "success" $ f [(1,2),(3,4)] `shouldBe` EValue (3,[(3,4)])

        it "expect fail" $ f [(2,3)] `shouldSatisfy` isString
      )
      \b -> do
        it "empty" $ b empty `shouldSatisfy` isString

        it "success" $ b [(1,3)] `shouldBe` EValue (4,[(1,3)])

  describe "lensBiparse" do
    let lb = lensBiparse _1 (take 1 :: Biparser IdentityState [Int] EitherString EitherString () ()) :: Constructor ([Int],()) EitherString EitherString () ()

    describe "forward" do
      let f = runForwardC lb
      it "success" $ f ([1],()) `shouldBe` EValue ()
      it "fail" $ f ([1],()) `shouldBe` EValue ()

    describe "backward" do
      let b = runBackwardC lb ()
      it "success" $ b ([],()) `shouldBe` EValue ((), ([1], ()))

  describe "expose" do
    let e = expose :: Constructor s Identity Identity u s

    it "forward" $ runForwardC e 'c' `shouldBe` Identity 'c'

    it "backward" $ runBackwardC e () 'c' `shouldBe` Identity ('c','c')

--  describe "Unordered" do
--    fb @() "ISB"
--      (_ (unordered :: Construct EitherString EitherString ISB)
--      :: Iso LinesOnly (FM Ts) EitherString (Position Ts) ISB)
--      (\f -> do
--        describe "success" do
--          prop "success" \(i,s,b,t) -> forAll (shuffle [One i, Two s, Three b]) \h ->
--            f (startLineColumn (h <> t)) `shouldBe` Right (ISB i s b, Position 4 1 t)
--
--        describe "fail" do
--          xit "Only Two" $ f [Two "False", Two "True"] `shouldSatisfy` errorPosition 2 1
--          xit "Only One" $ f [One 0, One 1] `shouldSatisfy` errorPosition 1 1
--      )
--      \b -> do
--        prop "print all" \v@(ISB i s b') ->
--          b v `shouldBe` EValue (v, [One i, Two s, Three b'])
--
--type Ts = [TriSum Int String Bool]
--
--
--
--oneWU :: WrapUnwrap (TriSum a b c) a
--oneWU = WrapUnwrap (\case One x -> pure x; _ -> empty;) (pure . One)
--
--twoWU :: WrapUnwrap (TriSum a b c) b
--twoWU = WrapUnwrap (\case Two x -> pure x; _ -> empty;) (pure . Two)
--
--threeWU :: WrapUnwrap (TriSum a b c) c
--threeWU = WrapUnwrap (\case Three x -> pure x; _ -> empty;) (pure. Three)
--
--data ISB = ISB Int String Bool deriving (Show, Eq, Generic)
--
--instance Arbitrary ISB where
--  arbitrary = ISB <$> arbitrary <*> arbitrary <*> arbitrary
--  shrink (ISB i s b) = shrink (i,s,b) <&> \(x,y,z) -> ISB x y z
--
--instance (MonadPlus m, Alternative n, One c s m n Ts) => IsoClass c m n s String where
--  iso = wrappedOne twoWU
--
--instance (MonadPlus m, Alternative n, One c s m n Ts) => IsoClass c m n s Int where
--  iso = wrappedOne oneWU
--
--instance (MonadPlus m, Alternative n, One c s m n Ts) => IsoClass c m n s Bool where
--  iso = wrappedOne threeWU
--
