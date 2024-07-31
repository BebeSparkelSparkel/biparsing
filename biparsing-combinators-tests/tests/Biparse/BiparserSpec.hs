{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# OPTIONS_GHC -Wno-monomorphism-restriction #-}
module Biparse.BiparserSpec where

oneBP :: One a p => Iso p a
oneBP = one

peekOneBP :: (One a p, Peek (p a)) => Iso p a
peekOneBP = peek one

peekTupleBP :: (One a p, Peek (p a), Profunctor p, Applicative (p (a,a))) => Iso p (a,a)
peekTupleBP = (,) <$> peek one `upon` fst <*> one `upon` snd

peekAltBP :: (Peek (p Char), Try (p Char), Alt (p Char), MonadFail (p Char), One Char p) => Iso p Char
peekAltBP = peek (takeUni 'x') <!> takeUni 'a'

specForward :: forall (p :: Type -> Type -> Type) c m.
  ( One Char p
  , RunForward p FilePath String m c
  , ShouldReturn m
  , Typeable p
  , Profunctor p
  , forall a. Show a => ShouldFail (m a)
  , forall u. Applicative (p u)
  , forall u. Peek (p u)
  , forall u. Try (p u)
  , forall u. Alt (p u)
  , forall u. MonadFail (p u)
  , forall v. Eq v => Eq (c v)
  , forall v. Show v => Show (c v)
  , forall v. MakeForwardResult (Position () FilePath -> IndexPosition FilePath -> String -> v -> c v)
  ) => Proxy p -> Spec
specForward Proxy = describe (show $ typeRep @p) do
  describe "one" do
    let f = runForward @p oneBP
    it "success" let
      fp = "one-success-forward.test"
      in f fp "abc" `shouldReturn` makeForwardResult
          (Position @() fp 1 2)
          (IndexPosition fp 1)
          "bc"
          'a'
    it "none to take" let
      fp = "one-none-to-take-forward.test"
      in shouldFail $ f fp mempty
  describe "peek" do
    describe "peek one" do
      let f = runForward @p peekOneBP
      it "success" let
        fp = "peek-one-success-forward.test"
        in f fp "abc" `shouldReturn` makeForwardResult
            (Position @() fp 1 1)
            (IndexPosition fp 0)
            "abc"
            'a'
      it "fail" let
        fp = "peek-one-fail-forward.test"
        in shouldFail $ f fp ""
    it "peek tuple" let
      fp = "peek-tuple-forward.test"
      f = runForward @p peekTupleBP fp
      in f "abc" `shouldReturn` makeForwardResult
          (Position @() fp 1 2)
          (IndexPosition fp 1)
          "bc"
          ('a','a')
    describe "peek alt" do
      let f = runForward @p peekAltBP
      it "take" let
        fp = "peek-alt-take-forward.test"
        in f fp "xa" `shouldReturn` makeForwardResult
            (Position @() fp 1 1)
            (IndexPosition fp 0)
            "xa"
            'x'
      it "take fail" let
        fp = "peek-alt-take-fail-forward.test"
        in f fp "ab" `shouldReturn` makeForwardResult
            (Position @() fp 1 2)
            (IndexPosition fp 1)
            "b"
            'a'
      it "no match" let
        fp = "peek-alt-no-match-forward.test"
        in shouldFail $ f fp "b"

specBackward :: forall (p :: Type -> Type -> Type) m c.
  ( One Char p
  , Profunctor p
  , Peek (p Char)
  , forall u. Applicative (p u)
  , forall v. Show v => Show (c v)
  , forall v. Eq v => Eq (c v)
  , RunBackward p FilePath m c
  , ShouldReturn m
  , forall a. MakeBackwardResult (String -> a -> c a)
  ) => Spec
specBackward = describe "backward" do
  describe "one" do
    it "one" let
      fp = "one-backward.test"
      b = runBackward @p oneBP fp
      u = 'a'
      in b u `shouldReturn` makeBackwardResult
          "a"
          u
  describe "peek" do
    it "peek one" let
      fp = "peek-one-backward.test"
      b = runBackward @p peekOneBP fp
      u = 'a'
      in b u `shouldReturn` makeBackwardResult
        "a"
        u
    it "peek tuple" let
      fp = "peek-tuple-backward.test"
      b = runBackward @p peekTupleBP fp
      u = ('a','b')
      in b u `shouldReturn` makeBackwardResult
          "ab"
          u

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
----      ) )-- :: Iso ColumnsOnly (FM String) EitherString () Text () (Position () String) String)
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
--    describe "Alt" do
--      let bp :: (Peek (p Char), Try (p Char), Alt (p Char), MonadFail (p Char), One Char p) => Iso p Char
--          bp = peek (takeUni 'x') <!> takeUni 'a'
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
--    let bp :: (Try (p Char), One Char p, forall u. MonadFail (p u), Profunctor p) => Iso p Char
--        bp = try $ one <* take 'b'
--        f = runForward @(FM UnixLC Text) bp ()
--        b = runBackward @(BMIO Text) bp ()
--
--    describe "forward" do
--      it "success" $ f "abc" `shouldBe` Right ('a', (Position () 1 3, "c"))
--      
--      it "does not consume state in failed attempt" do
--        runForward @(FM UnixLC Text) (bp <!> takeUni 'c') () "cde" `shouldBe` Right ('c', (Position () 1 2, "de"))
--
--      it "fails if no alternate" do
--        f "" `shouldSatisfy` errorPosition 1 1
--
--    describe "backward" do
--        it "prints correctly" $ b 'a' >>= (`shouldBe` ('a',"ab"))
--
----        it "prints second if first fails (more of a test for the Biparser Alternative instance and should proabaly moved there)" do
----          x <- runBackward (setBackward bp (const empty) <!> bp) () () () 'z'
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
----      ( let x :: Biparser UnixLC (Position () Text) (FM Text) IO () Text () [Char] (Int,[Char])
----            x = count $ takeElementsWhile (== 'a')
----        in (x,x))
----      ()
----      ()
----      (\f -> do
----        prop "correct count" \(NonNegative x, NonNegative y) -> let
----          as :: (IsSequence a, Item' a ~ Char, Index a ~ Int) => a
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
----          as :: (IsSequence a, Item' a ~ Char, Index a ~ Int) => a
----          as = MT.replicate x 'a'
----          bs = MT.replicate y 'b'
----          in f _ -- (startLineColumn $ as <> bs) `shouldBe` Right ((fromIntegral x, as), Position () 1 (succ x) bs)
----      )
----      \b -> do
----        prop "correct count" \xs -> let
----          in b xs >>= (`shouldBe` ((fromIntegral $ olength xs, xs), xs))
--
--
----instance IsChar String where
----  fromChar = (: [])
----  toChar = undefined
--
