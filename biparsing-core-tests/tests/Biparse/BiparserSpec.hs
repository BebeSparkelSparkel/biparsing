{-# LANGUAGE TypeSynonymInstances #-}
module Biparse.BiparserSpec where

import Data.Sequences qualified as MT

spec :: Spec
spec = do
  describe "one" do
    describe "Identity" do
      fb "id"
        (one :: Iso () IO IO () String () (Identity String) Char)
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

      fb "tuple"
        ((,) <$> one `upon` fst <*> one `upon` snd :: Iso () IO IO () String () (Identity String) (Char,Char))
        ()
        ()
        (\f -> do
          it "used twice" do
            x <- f "abc"
            x `shouldBe` (('a','b'),"c")
        )
        \b -> do
          it "used twice" $ b ('a','b') >>= (`shouldBe` (('a','b'),"ab"))

    fb "LineColumn"
      (one :: Iso UnixLC (FM Text) IO () Text () (Position () Text) Char)
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

    fb "LineColumn [Text]"
      (one :: Iso LinesOnly (FM [Text]) IO () [Text] () (Position () [Text]) Text)
      ()
      ()
      (\f -> do
        it "empty" $ f [] `shouldSatisfy` errorPosition 1 1

        it "one" $ f ["abc","def"] `shouldBe` Right ("abc", Position () 2 1 ["def"])
      )
      \b -> do
        it "print string" $ b "abc" >>= (`shouldBe` ("abc", ["abc"]))

    fb "Differing parser and printer type"
      (one :: Iso UnixLC (FM ByteString) EitherString () ByteStringBuilder () (Position () ByteString) Word8)
      ()
      ()
      (\f -> do
        it "ByteString" do
          f "abc" `shouldBe` Right (fromChar 'a', Position () 1 2 "bc")
      )
      \b -> do
        it "Builder" do
          b (fromChar 'd') `shouldBe` EValue (fromChar 'd', "d")

    describe "Handle" $ do
      fb "as state"
        (one :: Iso UnixLC (FileM StrictByteString) (FileM StrictByteString) () (File StrictByteString) () (Position () (File StrictByteString)) Word8)
        ()
        ()
        (\f -> do
          it "empty" $ withFile "/dev/null" ReadMode \h ->
            runFileM h (f $ startLineColumn File) `shouldThrow` isUserError
        )
        \b -> do
          it "write character" do
            (_,h) <- openTempFile "/tmp" ""
            x <- runFileM h $ b $ fromChar 'a'
            x `shouldBe` (fromChar 'a', File)
            hSeek h AbsoluteSeek 0
            w <- hGetContents h
            w `shouldBe` "a"

      it "LineColumn" pending

  describe "split" do
    fb "Identity"
      -- take two
      (( split do
          x <- get
          y <- maybe (fail "") (pure . \(f,s) -> f:s:[]) $
            liftA2 (,) (headMay x) (MT.index x 1)
          put $ MT.drop 2 x
          return y
      ) :: Iso () IO IO () String () (Identity String) String)
      ()
      ()
      (\f -> do
        it "succeeds" $ f "abc" >>= (`shouldBe` ("ab", "c"))

        it "fails" $ f "a" `shouldThrow` isUserError
      )
      \b -> do
        it "mempty" $ b mempty >>= (`shouldBe` (mempty,mempty))

        it "prints all" $ b "abc" >>= (`shouldBe` ("abc","abc"))

    fb "Differing parser and printer type"
      (( split do
        s <- get
        put ""
        return s
      ) :: Iso ColumnsOnly (FM String) EitherString () Text () (Position () String) String)
      ()
      ()
      (\f -> do
        it "String" do
          f "abc" `shouldBe` Right ("abc", Position () 1 4 "")
      )
      \b -> do
        it "Text" do
          b "abc" `shouldBe` EValue ("abc" :: String, "abc" :: Text)

  describe "peek" do
    fb "simple"
      (peek one :: Iso () IO IO () String () (Identity String) Char)
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
      fb "Identity"
        (peek (takeUni 'x') <!> takeUni 'a' :: Iso () IO IO () String () (Identity String) Char)
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

      fb "LineColumn"
        (peek (takeUni 'x') <!> takeUni 'a' :: Iso UnixLC (FM String) IO () String () (Position () String) Char)
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
    let bp :: Biparser ColumnsOnly (Position () (Seq Char)) (FM (Seq Char)) IO () (Seq Char) () Char Char
        bp = try $ one <* take 'b'
        f = runForward bp
        b = runBackward bp () ()

    describe "forward" do
      it "success" $ f "abc" `shouldBe` Right ('a', Position () 1 3 "c")
      
      it "does not consume state in failed attempt" do
        runForward (bp <!> takeUni 'c') "cde" `shouldBe` Right ('c', Position () 1 2 "de")

      it "fails if no alternate" do
        f "" `shouldSatisfy` errorPosition 1 1

    describe "backward" do
        it "prints correctly" $ b 'a' >>= (`shouldBe` ('a',"ab"))

        it "prints second if first fails (more of a test for the Biparser Alternative instance and should proabaly moved there)" do
          x <- runBackward (setBackward bp (const empty) <!> bp) () () 'z'
          x `shouldBe` ('z',"zb")
      
  describe "isNull" do
    fb "Identity"
      (isNull :: ConstU () (Identity String) Identity Identity () String () [()] Bool)
      ()
      ()
      (\f -> do
        it "true" $ f mempty `shouldBe` Identity (True,mempty)

        it "false" $ f "a" `shouldBe` Identity (False,"a")
      )
      \b -> do
        it "true" $ b mempty `shouldBe` Identity (True,mempty)

        it "false" $ b [()] `shouldBe` Identity (False,mempty)

    fb "LineColumn"
      (isNull :: ConstU UnixLC (Position () String) Identity Identity () String () [()] Bool)
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
    fb "LineColumn"
      (breakWhen' $ stripPrefix "ab" :: Iso UnixLC (FM String) IO () ByteString () (Position () String) String)
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

    fb "Identity"
      (breakWhen' $ stripPrefix "ab" :: Iso () IO IO () String () (Identity String) String)
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
    fb "ElementContext" 
      (count $ takeElementsWhile (== 'a') :: Biparser UnixLC (Position () Text) (FM Text) IO () Text () [Char] (Natural,[Char]))
      ()
      ()
      (\f -> do
        prop "correct count" \(NonNegative x, NonNegative y) -> let
          as :: (IsSequence a, Element a ~ Char, Index a ~ Int) => a
          as = MT.replicate x 'a'
          bs = MT.replicate y 'b'
          in f (startLineColumn $ as <> bs) `shouldBe` Right ((fromIntegral x, as), Position () 1 (succ x) bs)
      )
      \b -> do
        prop "correct count" \xs -> let
          in b xs >>= (`shouldBe` ((fromIntegral $ length xs, xs), fromString xs))

    fb "SubStateContext" 
      (count $ takeWhile (== 'a') :: Biparser UnixLC (Position () Text) (FM Text) IO () Text () Text (Natural,Text))
      ()
      ()
      (\f -> do
        prop "correct count" \(NonNegative x, NonNegative y) -> let
          as :: (IsSequence a, Element a ~ Char, Index a ~ Int) => a
          as = MT.replicate x 'a'
          bs = MT.replicate y 'b'
          in f (startLineColumn $ as <> bs) `shouldBe` Right ((fromIntegral x, as), Position () 1 (succ x) bs)
      )
      \b -> do
        prop "correct count" \xs -> let
          in b xs >>= (`shouldBe` ((fromIntegral $ length xs, xs), xs))


instance IsChar String where
  fromChar = (: [])
  toChar = undefined

--instance ChangeMonad UnixLC (RWST () [String] () EitherString) (RWST () [Char] () EitherString) ([String] -> String) where
--  changeMonad' f = mapRWST $ _EValue . _3 %~ f
--type instance ChangeFunction UnixLC (RWST () [String] () EitherString) (RWST () [Char] () EitherString) = [String] -> String

