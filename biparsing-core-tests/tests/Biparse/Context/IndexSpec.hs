module Biparse.Context.IndexSpec where

spec :: Spec
spec = do
  it "one" do
    let bp :: Iso IndexContext (EISP String) EitherString () Text () (IndexPosition String) Char
        bp = one
    runForward bp "abcd" `shouldBe` Right ('a', IndexPosition 1 "bcd")
    runBackward bp () () () 'b' `shouldBe` EValue ('b', "b")

  it "take" do
    let bp :: Iso IndexContext (EISP ByteString) EitherString () ByteStringBuilder () (IndexPosition ByteString) ()
        bp = take 48
    runForward bp "0123" `shouldBe` Right ((), IndexPosition 1 "123")
    runBackward bp () () () () `shouldBe` EValue ((), "0")
