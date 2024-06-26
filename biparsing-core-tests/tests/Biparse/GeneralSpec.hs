module Biparse.GeneralSpec where

import Biparse.Text.Numeric (naturalBaseTen)
import Data.Sequences qualified as MT

spec :: Spec
spec = do
  describe "take" do
    describe "IdentityState" do
      fb "uni"
        (take 'a' :: Unit () (Identity Text) IO IO () Text ())
        ()
        ()
        ()
        (\f -> do
          it "take matching" $ f "abc" >>= (`shouldBe` ((), "bc"))

          describe "fail" do
            it "non-matching" $ f "bc" `shouldThrow` isUserError
          
            it "empty" $ f mempty `shouldThrow` isUserError
        )
        \b -> do
          it "print one" $ b () >>= (`shouldBe` ((), "a"))

      fb "di"
        (take 'a' *> take 'b' :: Unit () (Identity Text) IO IO () Text ())
        ()
        ()
        ()
        (\f -> do
          it "take two matching" $ f "abc" >>= (`shouldBe` ((), "c"))
        )
        \b -> do
          it "print two" $ b () >>= (`shouldBe` ((), "ab"))

    describe "LineColumn" do
      fb "uni"
        (take 'a' :: Unit UnixLC (Position () Text) (FM Text) Maybe () Text ())
        ()
        ()
        ()
        (\f -> do
          it "take matching" $ f "abc" `shouldBe` Right ((), Position () 1 2 "bc")

          describe "fail" do
            it "non-matching" $ f "bc" `shouldSatisfy` errorPosition 1 1
          
            it "empty" $ f "" `shouldSatisfy` errorPosition 1 1
        )
        \b -> do
          it "print one" $ b () `shouldBe` Just ((), "a")

      fb "di"
        (take 'a' *> take 'b' :: Unit UnixLC (Position () Text) (FM Text) Maybe () Text ())
        ()
        ()
        ()
        (\f -> do
          it "take two matching" $ f "abc" `shouldBe` Right ((), Position () 1 3 "c")

          describe "fail" do
            it "matches first but not second" $ f "ac" `shouldSatisfy` errorPosition 1 2

            it "matches first but finds end" $ f "a" `shouldSatisfy` errorPosition 1 2
        )
        \b -> do
          it "print two" $ b () `shouldBe` Just ((), "ab")

  fb "takeUni"
   (takeUni 'a' :: Iso UnixLC (FM Text) Maybe () Text () (Position () Text) Char)
   ()
   ()
   ()
   (\f -> do
     it "fail positon is correct" $ f "bc" `shouldSatisfy` errorPosition 1 1
   )
   \_ -> pure ()

  describe "takeDi" do
    fb "Identity"
      (takeDi 'x' 1 :: Iso () IO IO () Text () (Identity Text) Int)
      ()
      ()
      ()
      (\f -> do
        it "matches" $ f "xabc" >>= (`shouldBe` (1,"abc"))

        it "no matche" $ f "abc" `shouldThrow` isUserError
      )
      \_ -> pure ()

    fb "LineColumn"
      (takeDi 'x' 1 :: Iso UnixLC (FM Text) Maybe () Text () (Position () Text) Int)
      ()
      ()
      ()
      (\f -> do
        it "matches" $ f "xabc" `shouldBe` Right (1, Position () 1 2 "abc")

        it "no matche" $ f "abc" `shouldSatisfy` errorPosition 1 1
      )
      \_ -> pure ()

  describe "takeNot" do
    fb "Identity"
      (takeNot 'A' :: Iso () IO IO () String () (Identity String) Char)
      ()
      ()
      ()
      (\f -> do
        it "takes non-matching element" $ f "bc" >>= (`shouldBe` ('b', "c"))

        it "does not take matching element" $ f "Abc" `shouldThrow` isUserError
      )
      \b -> do
        it "prints non-matching" $ b 'c' >>= (`shouldBe` ('c', "c"))

        it "fails matching" do
          b 'A' `shouldThrow` isUserError

    fb "LineColumn"
      (takeNot 'A' :: Iso UnixLC (FM String) Maybe () String () (Position () String) Char)
      ()
      ()
      ()
      (\f -> do
        it "takes non-matching element" $ f "bc" `shouldBe` Right ('b', Position () 1 2 "c")

        it "does not take matching element" $ f "Abc" `shouldSatisfy` errorPosition 1 1
      )
      \b -> do
        it "prints non-matching" $ b 'c' `shouldBe` Just ('c', "c")

        it "fails matching" do
          b 'A' `shouldBe` Nothing

  fb "takeWhile"
    (takeWhile (/= 'x') :: Iso UnixLC (FM Text) IO () Text () (Position () Text) Text)
    ()
    ()
    ()
    (\f -> do
      it "empty" $ f "" `shouldBe` Right (mempty,"")

      it "take none" $ f "xab" `shouldBe` Right (mempty, "xab")

      it "take 2" $ f "abx" `shouldBe` Right ("ab", Position () 1 3 "x")

      it "take all" $ f "abc" `shouldBe` Right ("abc", Position () 1 4 mempty)
    )
    \b -> do
      it "empty" $ b mempty >>= (`shouldBe` (mempty,mempty))

      it "no x" $ b "abc" >>= (`shouldBe` ("abc", "abc"))

      it "has x" $ b "axc" >>= (`shouldBe` ("axc", "axc"))

  fb "drop"
    (drop (take 'a') :: Biparser ColumnsOnly (Position () (Vector Char)) (FM (Vector Char)) EitherString () Text () () ())
    ()
    ()
    ()
    (\f -> do
      it "empty" $ f "" `shouldBe` Right ((), Position () 1 1 "")
      it "one" $ f "a" `shouldBe` Right ((), Position () 1 2 "")
      it "many" $ f "aaaa" `shouldBe` Right ((), Position () 1 5 "")
      it "stops" $ f "aab" `shouldBe` Right ((), Position () 1 3 "b")
      it "no matches" $ f "bb" `shouldBe` Right ((), Position () 1 1 "bb")
    )
    \b -> do
      it "does not print anything" $ b () `shouldBe` EValue ((), "")

  fb "dropWhile"
    (dropWhile (== 1) :: Iso LinesOnly (FM (Vector Int)) EitherString () (Vector Int) () (Position () (Vector Int)) ())
    ()
    ()
    ()
    (\f -> do
      it "empty" $ f [] `shouldBe` Right ((), Position () 1 1 [])
      it "drop none" $ f [2,3] `shouldBe` Right ((), Position () 1 1 [2,3])
      it "drop all" $ f [1,1] `shouldBe` Right ((), Position () 3 1 [])
      it "drop some" $ f [1,1,2,3] `shouldBe` Right ((), Position () 3 1 [2,3])
    )
    \b -> do
      it "success" $ b () `shouldBe` EValue ((), mempty)

  fb "dropUntil"
    (dropUntil 'b' :: Iso ColumnsOnly (FM (Seq Char)) EitherString () (Seq Char) () (Position () (Seq Char)) ())
    ()
    ()
    ()
    (\f -> do
      it "empty" $ f "" `shouldSatisfy` errorPosition 1 1
      it "not found" $ f "aaa" `shouldSatisfy`errorPosition 1 4
      it "drop one" $ f "abb" `shouldBe` Right ((), Position () 1 3 "b")
      it "drop many" $ f "aaab" `shouldBe` Right ((), Position () 1 5 "")
    )
    \b -> do
      it "prints element" $ b () `shouldBe` EValue ((), "b")

  fb "skipUntil"
    (skipUntil $ (> 2) <$> one :: Const LinesOnly (Position () [Int]) (FM [Int]) EitherString () [Int] () Int)
    ()
    ()
    ()
    (\f -> do
      it "empty" $ f [] `shouldSatisfy` errorPosition 1 1

      it "no true" $ f [0, 1, 2] `shouldSatisfy` errorPosition 4 1

      it "success" $ f [0, 1, 2, 3, 4] `shouldBe` Right ((), Position () 4 1 [3, 4])
    )
    \b -> do
      it "success" $ b 5 `shouldBe` EValue ((), [5])

  fb "untilJust"
    (untilJust $ bool Nothing (Just ()) . (> 2) <$> one :: Biparser LinesOnly (Position () [Int]) (FM [Int]) EitherString () [Int] () Int ())
    ()
    ()
    ()
    (\f -> do
      it "empty" $ f [] `shouldSatisfy` errorPosition 1 1

      it "no Just" $ f [0, 1, 2] `shouldSatisfy` errorPosition 4 1

      it "success" $ f [0, 1, 2, 3, 4] `shouldBe` Right ((), Position () 5 1 [4])
    )
    \b -> do
      it "success" $ b 5 `shouldBe` EValue ((), [5])

  fb "pad"
    (pad 4 'x' naturalBaseTen :: Iso UnixLC (FM Text) IO () Text () (Position () Text) Int)
    ()
    ()
    ()
    (\f -> do
      it "no pad" do
        f "1" `shouldBe` Right (1, Position () 1 2 mempty)
        f "123" `shouldBe` Right (123, Position () 1 4 mempty)

      it "with pad" do
        f "x4" `shouldBe` Right (4, Position () 1 3 mempty)
        f "xxx456" `shouldBe` Right (456, Position () 1 7 mempty)

      it "empty fail" do
        f "" `shouldSatisfy` errorPosition 1 1

      describe "only pad" do
        it "one" $ f "x" `shouldSatisfy` errorPosition 1 2
        it "multiple" $ f "xxx" `shouldSatisfy` errorPosition 1 4
    )
    \b -> do
      it "add pad" do
        b 1 >>= (`shouldBe` (1, "xxx1"))
        b 123 >>= (`shouldBe` (123, "x123"))

      it "no pad exact" do
        b 1234 >>= (`shouldBe` (1234, "1234"))

      it "no pad over" do
        b 12345 >>= (`shouldBe` (12345, "12345"))

  fb "padSet"
    (padSet 4 'x' ['x','y'] naturalBaseTen :: Iso UnixLC (FM (Seq Char)) IO () (Seq Char) () (Position () (Seq Char)) Int)
    ()
    ()
    ()
    (\f -> do
      it "no pad" do
        f "1" `shouldBe` Right (1, Position () 1 2 mempty)
        f "123" `shouldBe` Right (123, Position () 1 4 mempty)

      it "with pad" do
        f "x4" `shouldBe` Right (4, Position () 1 3 mempty)
        f "xyx456" `shouldBe` Right (456, Position () 1 7 mempty)

      it "empty fail" do
        f "" `shouldSatisfy` errorPosition 1 1

      describe "only pad" do
        it "one" $ f "x" `shouldSatisfy` errorPosition 1 2
        it "multiple" $ f "xyx" `shouldSatisfy` errorPosition 1 4
    )
    \b -> do
      it "add pad" do
        b 1 >>= (`shouldBe` (1, "xxx1"))
        b 123 >>= (`shouldBe` (123, "x123"))

      it "no pad exact" do
        b 1234 >>= (`shouldBe` (1234, "1234"))

      it "no pad over" do
        b 12345 >>= (`shouldBe` (12345, "12345"))

  describe "padCount" do
    prop "forward" \(NonNegative (na :: Int), NonNegative (nb :: Int), NonNegative (nc :: Int), NonNegative (n :: Int)) -> let
      as = MT.replicate na 'a'
      as' = case n - nb of
        x | x >= 0 -> MT.replicate x 'a'
          | otherwise -> mempty
      bs = MT.replicate nb 'b'
      cs = MT.replicate nc 'c'
      bp :: Biparser () (Identity (Seq Char)) (FM (Seq Char)) (Either String) () (Seq Char) () (Seq Char) (Natural, Seq Char)
      bp = padCount n 'a' $ takeWhile (== 'b')
      f = runForward @() bp
      b = runBackward bp () () ()
      n' = fromIntegral n
      na' = fromIntegral na
      nb' = fromIntegral nb
      in do
        f (Identity $ as <> bs <> cs) `shouldBe` Right ((na' + nb', bs), Identity cs)
        b bs `shouldBe` Right ((n', bs), as' <> bs)

  fb "breakWhen"
    (breakWhen $ stripPrefix "ab" :: Iso UnixLC (FM (Seq Char)) IO () (Seq Char) () (Position () (Seq Char)) (Seq Char))
    ()
    ()
    ()
    (\f -> do
      it "empty" $ f "" `shouldSatisfy` errorPosition 1 1

      it "break first" do
        f "abcd" `shouldBe` Right (mempty, Position () 1 3 "cd")

      it "break last" do
        f "cdab" `shouldBe` Right ("cd", Position () 1 5 mempty)

      it "break middle" do
        f "cdabef" `shouldBe` Right ("cd", Position () 1 5 "ef")

      it "no break" do
        f "cdefg" `shouldSatisfy` errorPosition 1 6
    )
    \b -> do
      it "empty" $ b mempty >>= (`shouldBe` (mempty,"ab"))

      it "append break" $ b "cd" >>= (`shouldBe` ("cd", "cdab"))

      it "only break" $ b "ab" >>= (`shouldBe` ("ab", "abab"))

      it "contains break" $ b "cdab" >>= (`shouldBe` ("cdab", "cdabab"))

  fb "rest"
    (rest :: Iso () (FM (Identity (Vector Int))) EitherString () (Vector Int) () (Identity (Vector Int)) (Vector Int))
    ()
    ()
    ()
    (\f -> do
      it "success" $ f [1,2,3] `shouldBe` Right ([1,2,3], mempty)
    )
    \b -> do
      it "success" $ b [1,2,3] `shouldBe` EValue ([1,2,3], [1,2,3])

  fb "shouldFail"
    (shouldFail (take 1) "take 1 did not fail" :: Biparser ColumnsOnly (Position () [Int]) (FM [Int]) EitherString () [Int] () () ())
    ()
    ()
    ()
    (\f -> do
      it "empty" $ f [] `shouldBe` Right ((), Position () 1 1 [])
      it "take fails" $ f [2] `shouldBe` Right ((), Position () 1 1 [2])
      it "take succeeds" $ f [1] `shouldSatisfy` errorPosition 1 2
    )
    \b -> it "prints nothing" $ b () `shouldBe` EValue ((), [])

  let mapBool :: Bool -> Int
      mapBool = \case True -> 1; False -> 2
  fb "optionMaybe"
    ((,) <$> optionMaybe (takeUni 1 `upon` mapBool $> "one")
         <*> optionMaybe (takeUni 2 `upon` mapBool $> "two")
    :: Biparser () (Identity (Vector Int)) IO IO () (Vector Int) () Bool (Maybe String, Maybe String))
    ()
    ()
    ()
    (\f -> do
      it "matches both" do
        f [1, 2] >>= (`shouldBe` ((Just "one", Just "two"), mempty))

      it "matches first" do
        f [1, 3] >>= (`shouldBe` ((Just "one", Nothing), [3]))

      it "matches second" do
        f [2, 3] >>= (`shouldBe` ((Nothing, Just "two"), [3]))

      it "matches none" do
        f mempty >>= (`shouldBe` ((Nothing, Nothing), mempty))
    )
    \b -> do
      it "prints first" $ b True >>= (`shouldBe` ((Just "one", Nothing), [1]))

      it "prints second" $ b False >>= (`shouldBe` ((Nothing, Just "two"), [2]))

  describe "stripPrefix" do
    fb "Identity"
      (stripPrefix "abc" :: Unit () (Identity Text) IO IO () Text ())
      ()
      ()
      ()
      (\f -> do
        it "match" $ f "abcdef" >>= (`shouldBe` ((), "def"))

        it "no match" $ f "def" `shouldThrow` isUserError

        it "empty" $ f "" `shouldThrow` isUserError
      )
      \b -> do
        it "prints prefix" $ b () >>= (`shouldBe` ((), "abc"))
    
    fb "LineColumn"
      (stripPrefix "abc" :: Unit UnixLC (Position () Text) (FM Text) IO () Text ())
      ()
      ()
      ()
      (\f -> do
        it "match" $ f "abcdef" `shouldBe` Right ((), Position () 1 4 "def")

        it "no match" $ f "def" `shouldSatisfy` errorPosition 1 1

        it "empty" $ f "" `shouldSatisfy` errorPosition 1 1
      )
      \b -> do
        it "prints prefix" $ b () >>= (`shouldBe` ((), "abc"))

  fb "not"
    (not $ (== 'x') <$> one :: Biparser () (Identity String) IO IO () String () Char Bool)
    ()
    ()
    ()
    (\f -> do
      it "true" $ f "ab" >>= (`shouldBe` (True,"b"))

      it "false" $ f "xb" >>= (`shouldBe` (False,"b"))
    )
    \b -> do
      it "true" $ b 'x' >>= (`shouldBe` (False,"x"))

      it "false" $ b 'a' >>= (`shouldBe` (True,"a"))

