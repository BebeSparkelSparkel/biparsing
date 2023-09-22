module Biparse.GeneralSpec where

import Biparse.Text.Numeric (naturalBaseTen)

spec :: Spec
spec = do
  describe "take" do
    describe "IdentityState" do
      fb @() "uni"
        (take 'a' :: Unit IdentityState Text IO IO ())
        ()
        (\f -> do
          it "take matching" $ f "abc" >>= (`shouldBe` ((), "bc"))

          describe "fail" do
            it "non-matching" $ f "bc" `shouldThrow` isUserError
          
            it "empty" $ f mempty `shouldThrow` isUserError
        )
        \b -> do
          it "print one" $ b () >>= (`shouldBe` ((), "a"))

      fb @() "di"
        (take 'a' *> take 'b' :: Unit IdentityState Text IO IO ())
        ()
        (\f -> do
          it "take two matching" $ f "abc" >>= (`shouldBe` ((), "c"))
        )
        \b -> do
          it "print two" $ b () >>= (`shouldBe` ((), "ab"))

    describe "LineColumn" do
      fb @() "uni"
        (take 'a' :: Unit UnixLC (Position Text) (FM Text) Maybe ())
        ()
        (\f -> do
          it "take matching" $ f "abc" `shouldBe` Right ((), Position 1 2 "bc")

          describe "fail" do
            it "non-matching" $ f "bc" `shouldSatisfy` errorPosition 1 1
          
            it "empty" $ f "" `shouldSatisfy` errorPosition 1 1
        )
        \b -> do
          it "print one" $ b () `shouldBe` Just ((), "a")

      fb @() "di"
        (take 'a' *> take 'b' :: Unit UnixLC (Position Text) (FM Text) Maybe ())
        ()
        (\f -> do
          it "take two matching" $ f "abc" `shouldBe` Right ((), Position 1 3 "c")

          describe "fail" do
            it "matches first but not second" $ f "ac" `shouldSatisfy` errorPosition 1 2

            it "matches first but finds end" $ f "a" `shouldSatisfy` errorPosition 1 2
        )
        \b -> do
          it "print two" $ b () `shouldBe` Just ((), "ab")

  fb @() "takeUni"
   (takeUni 'a' :: Iso UnixLC (FM Text) Maybe () (Position Text) Char)
   ()
   (\f -> do
     it "fail positon is correct" $ f "bc" `shouldSatisfy` errorPosition 1 1
   )
   \_ -> pure ()

  describe "takeDi" do
    fb @() "IdentityState"
      (takeDi 'x' 1 :: Iso IdentityState IO IO () Text Int)
      ()
      (\f -> do
        it "matches" $ f "xabc" >>= (`shouldBe` (1,"abc"))

        it "no matche" $ f "abc" `shouldThrow` isUserError
      )
      \_ -> pure ()

    fb @() "LineColumn"
      (takeDi 'x' 1 :: Iso UnixLC (FM Text) Maybe () (Position Text) Int)
      ()
      (\f -> do
        it "matches" $ f "xabc" `shouldBe` Right (1, Position 1 2 "abc")

        it "no matche" $ f "abc" `shouldSatisfy` errorPosition 1 1
      )
      \_ -> pure ()

  describe "takeNot" do
    fb @() "IdentityState"
      (takeNot 'A' :: Iso IdentityState IO IO () String Char)
      ()
      (\f -> do
        it "takes non-matching element" $ f "bc" >>= (`shouldBe` ('b', "c"))

        it "does not take matching element" $ f "Abc" `shouldThrow` isUserError
      )
      \b -> do
        it "prints non-matching" $ b 'c' >>= (`shouldBe` ('c', "c"))

        it "fails matching" do
          b 'A' `shouldThrow` isUserError

    fb @() "LineColumn"
      (takeNot 'A' :: Iso UnixLC (FM String) Maybe () (Position String) Char)
      ()
      (\f -> do
        it "takes non-matching element" $ f "bc" `shouldBe` Right ('b', Position 1 2 "c")

        it "does not take matching element" $ f "Abc" `shouldSatisfy` errorPosition 1 1
      )
      \b -> do
        it "prints non-matching" $ b 'c' `shouldBe` Just ('c', "c")

        it "fails matching" do
          b 'A' `shouldBe` Nothing

  fb @() "takeWhile"
    (takeWhile (/= 'x') :: Iso UnixLC (FM Text) IO () (Position Text) Text)
    ()
    (\f -> do
      it "empty" do
        f "" `shouldBe` Right (mempty,"")

      it "take none" do
        f "xab" `shouldBe` Right (mempty, "xab")

      it "take 2" do
        f "abx" `shouldBe` Right ("ab", Position 1 3 "x")

      it "take all" do
        f "abc" `shouldBe` Right ("abc", Position 1 4 mempty)
    )
    \b -> do
      it "empty" $ b mempty >>= (`shouldBe` (mempty,mempty))

      it "no x" $ b "abc" >>= (`shouldBe` ("abc", "abc"))

      it "has x" $ b "axc" >>= (`shouldBe` ("axc", "axc"))

  fb @() "pad"
    (pad 4 'x' naturalBaseTen :: Iso UnixLC (FM Text) IO () (Position Text) Int)
    ()
    (\f -> do
      it "no pad" do
        f "1" `shouldBe` Right (1, Position 1 2 mempty)
        f "123" `shouldBe` Right (123, Position 1 4 mempty)

      it "with pad" do
        f "x4" `shouldBe` Right (4, Position 1 3 mempty)
        f "xxx456" `shouldBe` Right (456, Position 1 7 mempty)

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

  describe "padCount" do
    prop "forward" \(NonNegative (na :: Int), NonNegative (nb :: Int), NonNegative (nc :: Int), NonNegative (n :: Int)) -> let
      as = replicate na 'a'
      as' = case n - nb of
        x | x >= 0 -> replicate x 'a'
          | otherwise -> mempty
      bs = replicate nb 'b'
      cs = replicate nc 'c'
      bp :: Biparser IdentityState (Seq Char) (FM (Seq Char)) (Either String) () (Seq Char) (Natural, Seq Char)
      bp = padCount n' 'a' $ takeWhile (== 'b')
      f = runForward @() bp
      b = runBackward bp ()
      n' = convertIntegralUnsafe n
      na' = convertIntegralUnsafe na
      nb' = convertIntegralUnsafe nb
      in do
        f (as <> bs <> cs) `shouldBe` Right ((na' + nb', bs), cs)
        b bs `shouldBe` Right ((n', bs), as' <> bs)

  fb @() "breakWhen"
    (breakWhen $ stripPrefix "ab" :: Iso UnixLC (FM (Seq Char)) IO () (Position (Seq Char)) (Seq Char))
    ()
    (\f -> do
      it "empty" do
        f "" `shouldBe` Right (mempty, "")

      it "break first" do
        f "abcd" `shouldBe` Right (mempty, Position 1 3 "cd")

      it "break last" do
        f "cdab" `shouldBe` Right ("cd", Position 1 5 mempty)

      it "break middle" do
        f "cdabef" `shouldBe` Right ("cd", Position 1 5 "ef")

      it "no break" do
        f "cdefg" `shouldBe` Right ("cdefg", Position 1 6 mempty)
    )
    \b -> do
      it "empty" $ b mempty >>= (`shouldBe` (mempty,"ab"))

      it "append break" $ b "cd" >>= (`shouldBe` ("cd", "cdab"))

      it "only break" $ b "ab" >>= (`shouldBe` ("ab", "abab"))

      it "contains break" $ b "cdab" >>= (`shouldBe` ("cdab", "cdabab"))

  let mapBool :: Bool -> Int
      mapBool = \case True -> 1; False -> 2
  fb @() "optionMaybe"
    ((,) <$> optionMaybe (takeUni 1 `upon` mapBool $> "one")
         <*> optionMaybe (takeUni 2 `upon` mapBool $> "two")
    :: Biparser IdentityState (Vector Int) IO IO () Bool (Maybe String, Maybe String))
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
    fb @() "IdentityState"
      (stripPrefix "abc" :: Unit IdentityState Text IO IO ())
      ()
      (\f -> do
        it "match" $ f "abcdef" >>= (`shouldBe` ((), "def"))

        it "no match" $ f "def" `shouldThrow` isUserError

        it "empty" $ f "" `shouldThrow` isUserError
      )
      \b -> do
        it "prints prefix" $ b () >>= (`shouldBe` ((), "abc"))
    
    fb @() "LineColumn"
      (stripPrefix "abc" :: Unit UnixLC (Position Text) (FM Text) IO ())
      ()
      (\f -> do
        it "match" $ f "abcdef" `shouldBe` Right ((), Position 1 4 "def")

        it "no match" $ f "def" `shouldSatisfy` errorPosition 1 1

        it "empty" $ f "" `shouldSatisfy` errorPosition 1 1
      )
      \b -> do
        it "prints prefix" $ b () >>= (`shouldBe` ((), "abc"))

  fb @() "not"
    (not $ (== 'x') <$> one :: Biparser IdentityState String IO IO () Char Bool)
    ()
    (\f -> do
      it "true" $ f "ab" >>= (`shouldBe` (True,"b"))

      it "false" $ f "xb" >>= (`shouldBe` (False,"b"))
    )
    \b -> do
      it "true" $ b 'x' >>= (`shouldBe` (False,"x"))

      it "false" $ b 'a' >>= (`shouldBe` (True,"a"))

