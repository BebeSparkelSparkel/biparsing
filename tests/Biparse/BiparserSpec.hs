module Biparse.BiparserSpec where

import Biparse.Biparser (backward)
import Biparse.List (splitOn, many)
import Data.Sequence (Seq)
import Data.Sequence qualified as MT

-- functional tests that should be moved to General when implemented correctly
import Biparse.Biparser (breakWhen')

spec :: Spec
spec = do
  describe "one" do
    describe "IdentityStateContext" do
      let bp = one :: Iso IdentityStateContext IO IO String Char
          bp2 :: Iso IdentityStateContext IO IO String (Char,Char)
          bp2 = (,) <$> bp `upon` fst <*> bp `upon` snd

      describe "forward" do
        it "one use" do
          x <- runForward bp "abc"
          x `shouldBe` ('a',"bc")

        it "used twice" do
          x <- runForward bp2 "abc"
          x `shouldBe` (('a','b'),"c")
          
        it "none to take" do
          runForward bp mempty `shouldThrow` isUserError

      describe "backward" do
        it "typical use" do
          x <- runBackward bp 'a'
          x `shouldBe` ('a',"a")

        it "used twice" do
          x <- runBackward bp2 ('a','b')
          x `shouldBe` (('a','b'),"ab")

    fb "LineColumn"
      (one :: Iso LineColumn IO IO (Position Text) Char)
      (\f -> do
        it "empty" do
          f "" `shouldThrow` isUserError

        it "one" do
          x <- f "abc"
          x `shouldBe` ('a', Position 1 2 "bc")

      )
      \b -> do
        it "write char" do
          x <- b 'd'
          x `shouldBe` ('d', "d")

    fb "LineColumn [Text]"
      (one :: Iso LineColumn IO IO (Position [Text]) Text)
      (\f -> do
        it "empty" do
          f [] `shouldThrow` isUserError

        it "one" do
          x <- f ["abc","def"]
          x `shouldBe` ("abc", Position 2 1 ["def"])

      )
      \b -> do
        it "print string" do
          x <- b "abc"
          x `shouldBe` ("abc", ["abc"])

  describe "split" do
    let bp :: Iso IdentityStateContext IO IO String String
        bp = split do
          x <- get
          y <- maybe empty (pure . \(f,s) -> f:s:[]) $
            liftA2 (,) (headMay x) (index x 1)
          put $ drop 2 x
          return y

    describe "forward" do
      let f = runForward bp

      it "succeeds" $ f "abc" >>= (`shouldBe` ("ab", "c"))

      it "fails" $ f "a" `shouldThrow` isUserError

    describe "backward" do
      let b = runBackward bp

      it "mempty" $ b mempty >>= (`shouldBe` (mempty,mempty))

      it "prints all" $ b "abc" >>= (`shouldBe` ("abc","abc"))

  describe "peek" do
    describe "simple" do
      let bp :: Iso IdentityStateContext IO IO String Char
          bp = peek one

      describe "forward" do
        let f = runForward bp

        it "none consumed" do
          x <- f "abc"
          x `shouldBe` ('a',"abc")

      describe "backward" do
        let b = runBackward bp

        it "prints char" do
          x <- b 'a'
          x `shouldBe` ('a',"a")

    describe "Alternative" do
      let bp :: Iso IdentityStateContext IO IO String Char
          bp = peek (takeUni 'x') <|> takeUni 'a'

      describe "forward" do
        let f = runForward bp

        it "take first" do
          x <- f "xa"
          x `shouldBe` ('x',"xa")

        it "take second" do
          x <- f "ab"
          x `shouldBe` ('a',"b")

      describe "backward" do
        let b = runBackward bp

        it "prints first" do
          x <- b 'x'
          x `shouldBe` ('x',"x")

        it "prints second" do
          x <- b 'a'
          x `shouldBe` ('a',"a")

  describe "try" do
    let bp :: Biparser IdentityStateContext (Seq Char) IO IO Char Char
        bp = try $ one <* take 'b'

    describe "forward" do
      it "success" do
        x <- runForward bp "abc"
        x `shouldBe` ('a',"c")
      
      it "does not consume state in failed attempt" do
        x <- runForward (bp <|> takeUni 'c') "cde"
        x `shouldBe` ('c', "de")

      it "fails if no alternate" do
        runForward bp mempty `shouldThrow` isUserError

    describe "backward" do
      it "prints correctly" do
        x <- runBackward bp 'a'
        x `shouldBe` ('a',"ab")

      it "prints second if first fails (more of a test for the Biparser Alternative instance and should proabaly moved there)" do
        x <- runBackward (bp {backward = const empty} <|> bp) 'z'
        x `shouldBe` ('z',"zb")
      
  describe "isNull" do
    let bp :: ConstU IdentityStateContext String Identity Identity [()] Bool
        bp = isNull

    describe "forward" do
      let f = runForward bp

      it "true" $ f mempty `shouldBe` Identity (True,mempty)

      it "false" $ f "a" `shouldBe` Identity (False,"a")

    describe "backward" do
      let b = runBackward bp

      it "true" $ b mempty `shouldBe` Identity (True,mempty)

      it "false" $ b [()] `shouldBe` Identity (False,mempty)

  fb
    "breakWhen'"
    (breakWhen' $ stripPrefix "ab" :: Iso LineColumn IO IO (Position String) String)
    (\f -> do
      it "empty" do
        f "" `shouldThrow` isUserError

      it "break first" do
        x <- f "abcd"
        x `shouldBe` (mempty, Position 1 3 "cd")

      it "break last" do
        x <- f "cdab"
        x `shouldBe` ("cd", Position 1 5 mempty)

      it "break middle" do
        x <- f "cdabef"
        x `shouldBe` ("cd", Position 1 5 "ef")

      it "no break" do
        f "cdefg" `shouldThrow` isUserError
    )
    \b -> do
      it "empty" do
        x <- b mempty
        x `shouldBe` (mempty,"ab")

      it "append break" do
        x <- b "cd"
        x `shouldBe` ("cd", "cdab")

      it "only break" do
        x <- b "ab"
        x `shouldBe` ("ab", "abab")

      it "contains break" do
        x <- b "cdab"
        x `shouldBe` ("cdab", "cdabab")

