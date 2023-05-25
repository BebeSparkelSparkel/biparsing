module Biparse.ListSpec where

import Test.Hspec

spec :: Spec
spec = return ()
--spec = do
--  describe "many" do
--    let bp :: BiparserT [String] Maybe Maybe [Bool] [Int]
--        bp = many
--          $   try (take''' "TRUE" True 1)
--          <|>      take''' "FALSE" False 0 
--
--    describe "forward" do
--      it "takes two" do
--        runForward bp ["TRUE","FALSE","UNDEFINED"]
--          `shouldBe` Just ([1,0],["UNDEFINED"])
--
--      it "takes none" do
--        runForward bp ["UNDEFINED"]
--          `shouldBe` Just (mempty, ["UNDEFINED"])
--        runForward bp mempty
--          `shouldBe` Just (mempty, mempty)
--
--    describe "backward" do
--      it "prints all" do
--        runBackward bp [False, True]
--          `shouldBe` Just ([0,1], ["FALSE", "TRUE"])
--
--      it "prints none" do
--        runBackward bp mempty
--          `shouldBe` Just (mempty, mempty)

