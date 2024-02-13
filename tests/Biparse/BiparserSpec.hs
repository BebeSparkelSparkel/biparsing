module Biparse.BiparserSpec where

import Biparse.List (takeElementsWhile)

import Biparse.Biparser qualified as BB
import Biparse.Biparser.StateReaderWriter (N)
import Biparse.List (all)
import Biparse.Text.LineBreak (lines, LineBreakType(Unix))
import Control.Lens (_3, (%~))
import Control.Monad.EitherString (_EValue)
import Control.Monad.RWS (mapRWST)
import Data.Sequences qualified as MT
import Text.Printf (IsChar(fromChar, toChar))

spec :: Spec
spec = do
  describe "one" do
    describe "Identity" do
      fb @() "id"
        (one :: Iso () IO IO () () (Identity String) Char)
        ()
        ()
        (\f -> do
          it "one use" do
            x <- f "abc"
            x `shouldBe` ('a',"bc")

          it "none to take" do
            f mempty `shouldThrow` isUserError
        )
        \b -> do
          it "typical use" $ b 'a' >>= (`shouldBe` ('a',"a"))

      fb @() "tuple"
        ((,) <$> one `upon` fst <*> one `upon` snd :: Iso () IO IO () () (Identity String) (Char,Char))
        ()
        ()
        (\f -> do
          it "used twice" do
            x <- f "abc"
            x `shouldBe` (('a','b'),"c")
        )
        \b -> do
          it "used twice" $ b ('a','b') >>= (`shouldBe` (('a','b'),"ab"))

    fb @() "LineColumn"
      (one :: Iso UnixLC (FM Text) IO () () (Position () Text) Char)
      ()
      ()
      (\f -> do
        it "empty" do
          f "" `shouldSatisfy` errorPosition 1 1

        it "one" do
          f "abc" `shouldBe` Right ('a', Position () 1 2 "bc")

      )
      \b -> do
        it "write char" $ b 'd' >>= (`shouldBe` ('d', "d"))

    fb @() "LineColumn [Text]"
      (one :: Iso LinesOnly (FM [Text]) IO () () (Position () [Text]) Text)
      ()
      ()
      (\f -> do
        it "empty" $ f [] `shouldSatisfy` errorPosition 1 1

        it "one" $ f ["abc","def"] `shouldBe` Right ("abc", Position () 2 1 ["def"])
      )
      \b -> do
        it "print string" $ b "abc" >>= (`shouldBe` ("abc", ["abc"]))

  describe "split" do
    let takeTwo :: Iso () IO IO () () (Identity String) String
        takeTwo = split do
          x <- get
          y <- maybe empty (pure . \(f,s) -> f:s:[]) $
            liftA2 (,) (headMay x) (index x 1)
          put $ drop 2 x
          return y

    fb @() "Identity"
      takeTwo
      ()
      ()
      (\f -> do
        it "succeeds" $ f "abc" >>= (`shouldBe` ("ab", "c"))

        it "fails" $ f "a" `shouldThrow` isUserError
      )
      \b -> do
        it "mempty" $ b mempty >>= (`shouldBe` (mempty,mempty))

        it "prints all" $ b "abc" >>= (`shouldBe` ("abc","abc"))

  describe "peek" do
    fb @() "simple"
      (peek one :: Iso () IO IO () () (Identity String) Char)
      ()
      ()
      (\f -> do
        it "none consumed" do
          x <- f "abc"
          x `shouldBe` ('a',"abc")
      )
      \b -> do
        it "prints char" $ b 'a' >>= (`shouldBe` ('a',"a"))

    describe "Alternative" do
      fb @() "Identity"
        (peek (takeUni 'x') <!> takeUni 'a' :: Iso () IO IO () () (Identity String) Char)
        ()
        ()
        (\f -> do
          it "take first" do
            x <- f "xa"
            x `shouldBe` ('x',"xa")

          it "take second" do
            x <- f "ab"
            x `shouldBe` ('a',"b")

          it "no match" $ f "b" `shouldThrow` isUserError
        )
        \b -> do
          it "prints first" $ b 'x' >>= (`shouldBe` ('x',"x"))

          it "prints second" $ b 'a' >>= (`shouldBe` ('a',"a"))

      fb @() "LineColumn"
        (peek (takeUni 'x') <!> takeUni 'a' :: Iso UnixLC (FM String) IO () () (Position () String) Char)
        ()
        ()
        (\f -> do
          it "take first" $ f "xa" `shouldBe` Right ('x', Position () 1 1 "xa")

          it "take second" $ f "ab" `shouldBe` Right ('a', Position () 1 2 "b")

          it "no match" $ f "b" `shouldSatisfy` errorPosition 1 1
        )
        \b -> do
          it "prints first" $ b 'x' >>= (`shouldBe` ('x',"x"))

          it "prints second" $ b 'a' >>= (`shouldBe` ('a',"a"))

  describe "try" do
    let bp :: Biparser ColumnsOnly (Position () (Seq Char)) (FM (Seq Char)) IO () () Char Char
        bp = try $ one <* take 'b'
        f = runForward @() bp
        b = runBackward bp () ()

    describe "forward" do
      it "success" $ f "abc" `shouldBe` Right ('a', Position () 1 3 "c")
      
      it "does not consume state in failed attempt" do
        runForward @() (bp <!> takeUni 'c') "cde" `shouldBe` Right ('c', Position () 1 2 "de")

      it "fails if no alternate" do
        f "" `shouldSatisfy` errorPosition 1 1

    describe "backward" do
        it "prints correctly" $ b 'a' >>= (`shouldBe` ('a',"ab"))

        it "prints second if first fails (more of a test for the Biparser Alternative instance and should proabaly moved there)" do
          x <- runBackward (setBackward bp (const empty) <!> bp) () () 'z'
          x `shouldBe` ('z',"zb")
      
  describe "isNull" do
    fb @() "Identity"
      (isNull :: ConstU () (Identity String) Identity Identity () () [()] Bool)
      ()
      ()
      (\f -> do
        it "true" $ f mempty `shouldBe` Identity (True,mempty)

        it "false" $ f "a" `shouldBe` Identity (False,"a")
      )
      \b -> do
        it "true" $ b mempty `shouldBe` Identity (True,mempty)

        it "false" $ b [()] `shouldBe` Identity (False,mempty)

    fb @() "LineColumn"
      (isNull :: ConstU UnixLC (Position () String) Identity Identity () () [()] Bool)
      ()
      ()
      (\f -> do
        it "true" $ f "" `shouldBe` Identity (True,"")

        it "false" $ f "a" `shouldBe` Identity (False,"a")
      )
      \b -> do
        it "true" $ b mempty `shouldBe` Identity (True,mempty)

        it "false" $ b [()] `shouldBe` Identity (False,mempty)

  describe "breakWhen'" do
    fb @() "LineColumn"
      (breakWhen' $ stripPrefix "ab" :: Iso UnixLC (FM String) IO () () (Position () String) String)
      ()
      ()
      (\f -> do
        it "empty" $ limit $
          f "" `shouldSatisfy` errorPosition 1 1

        it "break first" $ limit $
          f "abcd" `shouldBe` Right (mempty, Position () 1 3 "cd")

        it "break last" $ limit $
          f "cdab" `shouldBe` Right ("cd", Position () 1 5 mempty)

        it "break middle" $ limit $
          f "cdabef" `shouldBe` Right ("cd", Position () 1 5 "ef")

        it "no break" $ limit $
          f "cdefg" `shouldSatisfy` errorPosition 1 1
      )
      \b -> do
        it "empty" $ b mempty >>= (`shouldBe` (mempty,"ab"))

        it "append break" $ b "cd" >>= (`shouldBe` ("cd", "cdab"))

        it "only break" $ b "ab" >>= (`shouldBe` ("ab", "abab"))

        it "contains break" $ b "cdab" >>= (`shouldBe` ("cdab", "cdabab"))

    fb @() "Identity"
      (breakWhen' $ stripPrefix "ab" :: Iso () IO IO () () (Identity String) String)
      ()
      ()
      (\f -> do
        it "empty" $ limit $
          f "" `shouldThrow` isUserError

        it "break first" $ limit $
          f "abcd" >>= (`shouldBe` (mempty, "cd"))

        it "break last" $ limit $
          f "cdab" >>= (`shouldBe` ("cd", mempty))

        it "break middle" $ limit $
          f "cdabef" >>= (`shouldBe` ("cd", "ef"))

        it "no break" $ limit $
          f "cdefg" `shouldThrow` isUserError
      )
      \b -> do
        it "empty" $ b mempty >>= (`shouldBe` (mempty,"ab"))

        it "append break" $ b "cd" >>= (`shouldBe` ("cd", "cdab"))

        it "only break" $ b "ab" >>= (`shouldBe` ("ab", "abab"))

        it "contains break" $ b "cdab" >>= (`shouldBe` ("cdab", "cdabab"))

  describe "count" do
    fb @() "ElementContext" 
      (count $ takeElementsWhile (== 'a') :: Biparser UnixLC (Position () Text) (FM Text) IO () () [Char] (Natural,[Char]))
      ()
      ()
      (\f -> do
        prop "correct count" \(NonNegative x, NonNegative y) -> let
          as :: (IsSequence a, Element a ~ Char, Index a ~ Int) => a
          as = replicate x 'a'
          bs = replicate y 'b'
          in f (startLineColumn $ as <> bs) `shouldBe` Right ((fromIntegral x, as), Position () 1 (x + 1) bs)
      )
      \b -> do
        prop "correct count" \xs -> let
          in b xs >>= (`shouldBe` ((fromIntegral $ length xs, xs), fromString xs))

    fb @() "SubStateContext" 
      (count $ takeWhile (== 'a') :: Biparser UnixLC (Position () Text) (FM Text) IO () () Text (Natural,Text))
      ()
      ()
      (\f -> do
        prop "correct count" \(NonNegative x, NonNegative y) -> let
          as :: (IsSequence a, Element a ~ Char, Index a ~ Int) => a
          as = replicate x 'a'
          bs = replicate y 'b'
          in f (startLineColumn $ as <> bs) `shouldBe` Right ((fromIntegral x, as), Position () 1 (x + 1) bs)
      )
      \b -> do
        prop "correct count" \xs -> let
          in b xs >>= (`shouldBe` ((fromIntegral $ length xs, xs), xs))

  describe "zoom" do

    fb @() "Biparser success"
      (BB.zoom @UnixLC @UnixLC @UnixLC @'(StateErrorT,Either) @(N UnixLC (Position () [String]) EitherString () ())
        (lines @'Unix)
        (all $ MT.reverse <$> one)
      :: Iso UnixLC (FM String) EitherString () () (Position () String) [String])
      ()
      ()
      (\f -> do
        it "success" $ f "abc\ndef\nghi" `shouldBe` Right (["cba","fed","ihg"], Position () 3 4 mempty)
      )
      \b -> do
        it "success" $ b ["cba","fed","ihg"] `shouldBe` EValue (["abc","def","ghi"], "cba\nfed\nihg")

    it "Biparser fail" pending


instance IsChar String where
  fromChar = (: [])
  toChar = undefined

instance ChangeMonad UnixLC (RWST () [String] () EitherString) (RWST () [Char] () EitherString) where
  changeMonad' f = mapRWST $ _EValue . _3 %~ f
type instance ChangeFunction UnixLC (RWST () [String] () EitherString) (RWST () [Char] () EitherString) = [String] -> String

