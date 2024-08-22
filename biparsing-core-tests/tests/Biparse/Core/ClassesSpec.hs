{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-monomorphism-restriction #-}

{-# OPTIONS_GHC -Wno-redundant-constraints #-}
module Biparse.Core.ClassesSpec (spec) where

spec :: Spec
spec = runAllTests @() @() @() @() @() @() @TestSuite testSuite

oneBP :: One a p => Iso p a
oneBP = one

peekOneBP :: (One a p, Peek (p a)) => Iso p a
peekOneBP = peek one

peekTupleBP :: (One a p, Peek (p a), Profunctor p, forall u. Applicative (p u)) => Iso p (a,a)
peekTupleBP = (,) <$> peek one `upon` fst <*> one `upon` snd

peekAltBP :: (Peek (p char), Try (p char), Alternative (p char), MonadFail (p char), One char p, Show char, Eq char, IsChar char) => Iso p char
peekAltBP = peek (takeUni (fromChar 'x')) <|> takeUni (fromChar 'a')

type TestSuite :: (Type -> Type -> Type) -> Constraint
class TestSuite p where testSuite :: Proxy p -> Spec
instance
  ( One char p
  , Profunctor p
  , forall u. ShouldReturnQ p u
  , forall u. Applicative (p u)
  , forall u. Peek (p u)
  , Typeable p
  , forall u a. Show a => ShowStM' (p u) a
  , forall u a. Eq a => EqStM' (p u) a
  , forall u. RunBase (TestParameters r s u String) (p u)
  , forall u. ConstructParameter u String (TestParameters r s u String)
  , r ~ Read (p ())
  , s ~ State (p ())
  , Show char
  , Eq char
  , IsChar char
  , forall u. ShouldFailQ p u
  , forall u. Try (p u)
  , forall u. Alternative (p u)
  , forall u. MonadFail (p u)
  , forall u. MakeIsoResult direction p u
  , ForwardOnly direction
  , direction ~ WhichDirection (p ())
  ) => TestSuite p where
  testSuite _ = describe (show $ typeRep @p) do
    let runForward :: forall u v. Biparser p u v -> FilePath -> u -> String -> BaseMonad (p u) (StM' (p u) v)
        runForward = run @p @r @s
    describe "one" do
      let f = runForward oneBP
      it "success" let
        fp = "one-success-forward.test"
        u = fromChar @char 'a'
        in f fp u "abc" `shouldReturn` makeResult @direction
            (Position @() fp 1 2)
            (IndexPosition fp 1)
            "bc"
            "a"
            u
      forwardOnly @direction $ it "none to take" let
        fp = "one-none-to-take-forward.test"
        in shouldFail $ f fp undefined ""
    describe "peek" do
      describe "peek one" do
        let f = runForward peekOneBP
        it "success" let
          fp = "peek-one-success-forward.test"
          u = fromChar @char 'a'
          in f fp u "abc" `shouldReturn` makeResult @direction
              (Position @() fp 1 1)
              (IndexPosition fp 0)
              "abc"
              "a"
              u
        forwardOnly @direction $ it "fail" let
          fp = "peek-one-fail-forward.test"
          in shouldFail $ f fp undefined ""
      it "peek tuple" let
        fp = "peek-tuple-forward.test"
        f = runForward peekTupleBP fp
        u = (fromChar @char 'a', fromChar @char 'a')
        in f u "abc" `shouldReturn` makeResult @direction
            (Position @() fp 1 2)
            (IndexPosition fp 1)
            "bc"
            "aa"
            u
      describe "peek alt" do
        let f = runForward peekAltBP
        it "take" let
          fp = "peek-alt-take-forward.test"
          u = fromChar @char 'x'
          in f fp u "xa" `shouldReturn` makeResult @direction
              (Position @() fp 1 1)
              (IndexPosition fp 0)
              "xa"
              "x"
              u
        it "take fail" let
          fp = "peek-alt-take-fail-forward.test"
          u = fromChar @char 'a'
          in f fp u "ab" `shouldReturn` makeResult @direction
              (Position @() fp 1 2)
              (IndexPosition fp 1)
              "b"
              "a"
              u
        it "no match" let
          fp = "peek-alt-no-match-forward.test"
          in shouldFail $ f fp (fromChar @char 'b') "b"

--specBackward :: forall (p :: Type -> Type -> Type) char r s.
--  ( One char p
--  , Profunctor p
--  , forall u. Applicative (p u)
--  , forall u. ShouldReturnQ p u
--  , forall u v. MakeBackwardResult v (p u) v
--  , forall u a. Show a => ShowStM' (p u) a
--  , forall u a. Eq a => EqStM' (p u) a
--  , Show char
--  , IsChar char
--  , Eq char
--  , forall u. RunBase (TestParameters 'Backward r s u) (p u)
--  , forall u. ConstructParameter u r
--  , forall u. ConstructParameter u s
--  , forall u. Peek (p u)
--  , Typeable p
--  ) => Spec
--specBackward = describe (show $ typeRep @p) do
--  describe "one" do
--    it "one" let
--      fp = "one-backward.test"
--      b = run @'Backward @p @r @s oneBP fp
--      u = fromChar @char 'a'
--      in b u `shouldReturn` makeResult
--          "a"
--          u
--  describe "peek" do
--    it "peek one" let
--      fp = "peek-one-backward.test"
--      b = run @'Backward @p @r @s peekOneBP fp
--      u = fromChar @char 'a'
--      in b u `shouldReturn` makeResult
--        "a"
--        u
--    it "peek tuple" let
--      fp = "peek-tuple-backward.test"
--      b = run @'Backward @p @r @s peekTupleBP fp
--      u = (fromChar @char 'a', fromChar @char 'b')
--      in b u `shouldReturn` makeResult
--          "ab"
--          u

----  describe "split" do
----    fb "Identity"
----      -- take two
----      (( split do
----          x <- get
----          y <- maybe (fail "") (pure . \(f,s) -> f:s:[]) $
----            liftA2 (,) (headMay x) (MT.index x 1)
----          put $ MT.drop 2 x
----          return y
----      ) )-- :: Iso () IO IO () String () (Identity String) String)
----      ()
----      ()
----      (\f -> do
----        it "succeeds" $ f "abc" >>= (`shouldBe` ("ab", "c"))
----
----        it "fails" $ f "a" `shouldThrow` isUserError
----      )
----      \b -> do
----        it "mempty" $ b mempty >>= (`shouldBe` (mempty,mempty))
----
----        it "prints all" $ b "abc" >>= (`shouldBe` ("abc","abc"))
----
----    fb "Differing parser and printer type"
----      (( split do
----        s <- get
----        put ""
----        return s
----      ) )-- :: Iso ColumnsOnly (FM String) (Either String) () Text () (Position () String) String)
----      ()
----      ()
----      (\f -> do
----        it "String" do
----          f "abc" `shouldBe` Right ("abc", Position () 1 4 "")
----      )
----      \b -> do
----        it "Text" do
----          b "abc" `shouldBe` EValue ("abc" :: String, "abc" :: Text)
--
--  describe "peek" do
--    fb @(FMIO () Text) @(BMIO Text) "simple"
--      ( let x :: (forall u. Peek (p u), One a p) => Iso p a
--            x = peek one
--        in (x,x))
--      ()
--      ()
--      (\f -> do
--        it "none consumed" do
--          x <- f "abc"
--          x `shouldBe` ('a', (Position () 1 1, "abc"))
--      )
--      \b -> do
--        it "prints char" $ b 'a' >>= (`shouldBe` ('a',"a"))
--
--    describe "Alternative" do
--      let bp :: (Peek (p char), Try (p char), Alternative (p char), MonadFail (p char), One char p) => Iso p char
--          bp = peek (takeUni 'x') <|> takeUni 'a'
--
--      fb @(FMIO UnixLC Text) @(BMIO Text) "Identity"
--        (bp,bp)
--        ()
--        ()
--        (\f -> do
--          it "take first" do
--            x <- f "xa"
--            x `shouldBe` ('x', (Position () 1 1, "xa"))
--
--          it "take second" do
--            x <- f "ab"
--            x `shouldBe` ('a', (Position () 1 2, "b"))
--
--          it "no match" $ f "b" `shouldThrow` isUserError
--        )
--        \b -> do
--          it "prints first" $ b 'x' >>= (`shouldBe` ('x',"x"))
--
--          it "prints second" $ b 'a' >>= (`shouldBe` ('a',"a"))
--
--      fb @(FM UnixLC Text) @(BM Text) "LineColumn"
--        (bp,bp)
--        ()
--        ()
--        (\f -> do
--          it "take first" $ f "xa" `shouldBe` Right ('x', (Position () 1 1, "xa"))
--
--          it "take second" $ f "ab" `shouldBe` Right ('a', (Position () 1 2, "b"))
--
--          it "no match" $ f "b" `shouldSatisfy` errorPosition 1 1
--        )
--        \b -> do
--          it "prints first" $ b 'x' `shouldBe` Right ('x',"x")
--
--          it "prints second" $ b 'a' `shouldBe` Right ('a',"a")
--
--  describe "try" do
--    let bp :: (Try (p char), One char p, forall u. MonadFail (p u), Profunctor p) => Iso p char
--        bp = try $ one <* take 'b'
--        f = run @(FM UnixLC Text) bp ()
--        b = runBackward @(BMIO Text) bp ()
--
--    describe "forward" do
--      it "success" $ f "abc" `shouldBe` Right ('a', (Position () 1 3, "c"))
--      
--      it "does not consume state in failed attempt" do
--        run @(FM UnixLC Text) (bp <|> takeUni 'c') () "cde" `shouldBe` Right ('c', (Position () 1 2, "de"))
--
--      it "fails if no alternate" do
--        f "" `shouldSatisfy` errorPosition 1 1
--
--    describe "backward" do
--        it "prints correctly" $ b 'a' >>= (`shouldBe` ('a',"ab"))
--
----        it "prints second if first fails (more of a test for the Biparser Alternative instance and should proabaly moved there)" do
----          x <- runBackward (setBackward bp (const empty) <|> bp) () () () 'z'
----          x `shouldBe` ('z',"zb")
--      
----  describe "isNull" do
----    fb "Identity"
----      ( let x :: ConstU () (Identity String) Identity Identity () String () [()] Bool
----            x = isNull
----        in (x,x))
----      ()
----      ()
----      (\f -> do
----        it "true" $ f mempty `shouldBe` Identity (True,mempty)
----
----        it "false" $ f "a" `shouldBe` Identity (False,"a")
----      )
----      \b -> do
----        it "true" $ b mempty `shouldBe` Identity (True,mempty)
----
----        it "false" $ b [()] `shouldBe` Identity (False,mempty)
----
----    fb "LineColumn"
----      ( let x :: ConstU UnixLC (Position () String) Identity Identity () String () [()] Bool
----            x = isNull
----        in (x,x))
----      ()
----      ()
----      (\f -> do
----        it "true" $ f "" `shouldBe` Identity (True,"")
----
----        it "false" $ f "a" `shouldBe` Identity (False,"a")
----      )
----      \b -> do
----        it "true" $ b mempty `shouldBe` Identity (True,mempty)
----
----        it "false" $ b [()] `shouldBe` Identity (False,mempty)
----
----  describe "breakWhen'" do
----    fb "LineColumn"
----      ( let x :: Iso UnixLC (FM String) IO () ByteString () (Position () String) String
----            x = breakWhen' $ stripPrefix "ab"
----        in (x,x))
----      ()
----      ()
----      (\f -> do
----        it "empty" $
----          f "" `shouldSatisfy` errorPosition 1 1
----
----        it "break first" $
----          f "abcd" `shouldBe` Right (mempty, Position () 1 3 "cd")
----
----        it "break last" $
----          f "cdab" `shouldBe` Right ("cd", Position () 1 5 mempty)
----
----        it "break middle" $
----          f "cdabef" `shouldBe` Right ("cd", Position () 1 5 "ef")
----
----        it "no break" $
----          f "cdefg" `shouldSatisfy` errorPosition 1 1
----      )
----      \b -> do
----        it "empty" $ b mempty >>= (`shouldBe` (mempty,"ab"))
----
----        it "append break" $ b "cd" >>= (`shouldBe` ("cd", "cdab"))
----
----        it "only break" $ b "ab" >>= (`shouldBe` ("ab", "abab"))
----
----        it "contains break" $ b "cdab" >>= (`shouldBe` ("cdab", "cdabab"))
----
----    fb "Identity"
----      ( let x :: Iso () IO IO () String () (Identity String) String
----            x = breakWhen' $ stripPrefix "ab"
----        in (x,x))
----      ()
----      ()
----      (\f -> do
----        it "empty" $
----          f "" `shouldThrow` isUserError
----
----        it "break first" $
----          f "abcd" >>= (`shouldBe` (mempty, "cd"))
----
----        it "break last" $
----          f "cdab" >>= (`shouldBe` ("cd", mempty))
----
----        it "break middle" $
----          f "cdabef" >>= (`shouldBe` ("cd", "ef"))
----
----        it "no break" $
----          f "cdefg" `shouldThrow` isUserError
----      )
----      \b -> do
----        it "empty" $ b mempty >>= (`shouldBe` (mempty,"ab"))
----
----        it "append break" $ b "cd" >>= (`shouldBe` ("cd", "cdab"))
----
----        it "only break" $ b "ab" >>= (`shouldBe` ("ab", "abab"))
----
----        it "contains break" $ b "cdab" >>= (`shouldBe` ("cdab", "cdabab"))
----
----  describe "count" do
----    fb "ElementContext" 
----      ( let x :: Biparser UnixLC (Position () Text) (FM Text) IO () Text () [char] (Int,[char])
----            x = count $ takeElementsWhile (== 'a')
----        in (x,x))
----      ()
----      ()
----      (\f -> do
----        prop "correct count" \(NonNegative x, NonNegative y) -> let
----          as :: (IsSequence a, Item' a ~ char, Index a ~ Int) => a
----          as = MT.replicate x 'a'
----          bs = MT.replicate y 'b'
----          in f (startLineColumn $ as <> bs) `shouldBe` Right ((fromIntegral x, as), Position () 1 (succ x) bs)
----      )
----      \b -> do
----        prop "correct count" \xs -> let
----          in b xs >>= (`shouldBe` ((fromIntegral $ olength xs, xs), fromString xs))
----
----    fb "SubStateContext" 
----      ( let x :: Biparser UnixLC (Position () Text) (FM Text) IO () Text () Text (Int,Text)
----            x = count $ takeWhile (== 'a')
----        in (x,x))
----      ()
----      ()
----      (\f -> do
----        prop "correct count" \(NonNegative x, NonNegative y) -> let
----          as :: (IsSequence a, Item' a ~ char, Index a ~ Int) => a
----          as = MT.replicate x 'a'
----          bs = MT.replicate y 'b'
----          in f _ -- (startLineColumn $ as <> bs) `shouldBe` Right ((fromIntegral x, as), Position () 1 (succ x) bs)
----      )
----      \b -> do
----        prop "correct count" \xs -> let
----          in b xs >>= (`shouldBe` ((fromIntegral $ olength xs, xs), xs))
--
--
----instance Ischar String where
----  fromchar = (: [])
----  tochar = undefined
--
