{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# OPTIONS_GHC -Wno-monomorphism-restriction #-}

{-# OPTIONS_GHC -Wno-redundant-constraints #-}
module Biparse.BiparserSpec where

spec :: Spec
spec = do
  specForwardsImpure @String     @IO @()
  specForwardsImpure @Text       @IO @()
  specForwardsImpure @ByteString @IO @()

  specForwardsPure @String     @IO    @() @()  @()
  specForwardsPure @Text       @IO    @() @()  @()
  specForwardsPure @ByteString @IO    @() @()  @()
  specForwardsPure @String     @Maybe @() @()  @()
  specForwardsPure @Text       @Maybe @() @()  @()
  specForwardsPure @ByteString @Maybe @() @()  @()

  specForwardsPure @String     @IO    @() @()  @(Position () ())
  specForwardsPure @Text       @IO    @() @()  @(Position () ())
  specForwardsPure @ByteString @IO    @() @()  @(Position () ())
  specForwardsPure @String     @Maybe @() @()  @(Position () ())
  specForwardsPure @Text       @Maybe @() @()  @(Position () ())
  specForwardsPure @ByteString @Maybe @() @()  @(Position () ())

  specForwardsPure @String     @IO    @() @()  @(IndexPosition ())
  specForwardsPure @Text       @IO    @() @()  @(IndexPosition ())
  specForwardsPure @ByteString @IO    @() @()  @(IndexPosition ())
  specForwardsPure @String     @Maybe @() @()  @(IndexPosition ())
  specForwardsPure @Text       @Maybe @() @()  @(IndexPosition ())
  specForwardsPure @ByteString @Maybe @() @()  @(IndexPosition ())

  specBackward @(Bwd (FileT (Backward String) IO)) @Char @() @()
  specBackward @(Bwd (FileT (Backward Text) IO)) @Char @() @()
  specBackward @(Bwd (FileT (Backward ByteString) IO)) @Word8 @() @()

  specBackward @(Bwd (FileT (Backward String) (IdentityT IO))) @Char @() @()
  specBackward @(Bwd (FileT (Backward Text) (IdentityT IO))) @Char @() @()
  specBackward @(Bwd (FileT (Backward ByteString) (IdentityT IO))) @Word8 @() @()

  specBackward @(Bwd (FileT (Backward String) (ReaderT () IO))) @Char @() @()
  specBackward @(Bwd (FileT (Backward Text) (ReaderT () IO))) @Char @() @()
  specBackward @(Bwd (FileT (Backward ByteString) (ReaderT () IO))) @Word8 @() @()

  specBackward @(Bwd (FileT (Backward String) (LazyWriterT () IO))) @Char @() @()
  specBackward @(Bwd (FileT (Backward Text) (LazyWriterT () IO))) @Char @() @()
  specBackward @(Bwd (FileT (Backward ByteString) (LazyWriterT () IO))) @Word8 @() @()

  specBackward @(Bwd (FileT (Backward String) (LazyStateT () IO))) @Char @() @()
  specBackward @(Bwd (FileT (Backward Text) (LazyStateT () IO))) @Char @() @()
  specBackward @(Bwd (FileT (Backward ByteString) (LazyStateT () IO))) @Word8 @() @()

  specBackward @(Bwd (FileT (Backward String) (LazyRWST () () () IO))) @Char @() @()
  specBackward @(Bwd (FileT (Backward Text) (LazyRWST () () () IO))) @Char @() @()
  specBackward @(Bwd (FileT (Backward ByteString) (LazyRWST () () () IO))) @Word8 @() @()

  specBackward @(Bwd (LazyWriterT String IO)) @Char @() @()
  specBackward @(Bwd (LazyRWST () String () IO)) @Char @() @()
  specBackward @(Bwd (LazyWriterT String Maybe)) @Char @() @()
  specBackward @(Bwd (LazyRWST () String () Maybe)) @Char @() @()

  specBackward @(Bwd (LazyWriterT Text IO)) @Char @() @()
  specBackward @(Bwd (LazyRWST () Text () IO)) @Char @() @()
  specBackward @(Bwd (LazyWriterT Text Maybe)) @Char @() @()
  specBackward @(Bwd (LazyRWST () Text () Maybe)) @Char @() @()

  specBackward @(Bwd (LazyWriterT ByteString IO)) @Word8 @() @()
  specBackward @(Bwd (LazyRWST () ByteString () IO)) @Word8 @() @()
  specBackward @(Bwd (LazyWriterT ByteString Maybe)) @Word8 @() @()
  specBackward @(Bwd (LazyRWST () ByteString () Maybe)) @Word8 @() @()

oneBP :: One a p => Iso p a
oneBP = one

peekOneBP :: (One a p, Peek (p a)) => Iso p a
peekOneBP = peek one

peekTupleBP :: (One a p, Peek (p a), Profunctor p, forall u. Applicative (p u)) => Iso p (a,a)
peekTupleBP = (,) <$> peek one `upon` fst <*> one `upon` snd

peekAltBP :: (Peek (p char), Try (p char), Alt (p char), MonadFail (p char), One char p, Show char, Eq char, IsChar char) => Iso p char
peekAltBP = peek (takeUni (fromChar 'x')) <!> takeUni (fromChar 'a')

specForwardsImpure :: forall text m w r s char.
  ( Element text ~ char
  , OpenFrom text
  , Typeable text
  , Show text
  , Eq text
  , IsString text
  , IsSequence text
  , MonadIO m
  , MonadIO (BaseMonad m)
  , forall a. Show a => ShowStM' m a
  , forall a. Eq a => EqStM' m a
  , MonadMask m
  , MonadMask (BaseMonad m)
  , RunBase (TestParameters 'Forward r s String) m
  , OneFwd char (FileT text m)
  , ShouldReturn (BaseMonad m)
  , Typeable m
  , ConstructParameter String (TestParameters 'Forward r s String)
  , BaseMonad m ~ IO
  , r ~ Read m
  , s ~ State m
  , MonadState s m
  , UpdateStateWithElement s char
  , Show char
  , Eq char
  , IsChar char
  , MonadFileGetChar char
  , Typeable r
  , Show w
  , Eq w
  , Monoid w
  , Typeable w
  , Show s
  , Eq s
  , Typeable s
  , Peek m
  , forall v. MakeForwardResult m v v
  , forall v. MakeForwardResult m v (v, w)
  , forall v. MakeForwardResult m v (v, s)
  , forall v. MakeForwardResult m v (v, s, w)
  ) => Spec
specForwardsImpure = do
  specForward @(Fwd (FileT (Forward text) m))
  specForward @(Fwd (FileT (Forward text) (IdentityT m)))

  specForward @(Fwd (FileT (Forward text) (ReaderT   r m)))

  specForward @(Fwd (FileT (Forward text) (LazyWriterT   w m)))
  ----specForward @(Fwd (FileT (Forward text) (IdentityT (LazyWriterT   () m))))
  ----specForward @(Fwd (FileT (Forward text) (IdentityT (StrictWriterT () m))))
  ----specForward @(Fwd (FileT (Forward text) (IdentityT (StrictWriterT () m))))

  specForward @(Fwd (FileT (Forward text) (LazyStateT   s   m)))
  --specForward @(Fwd (FileT (Forward text) (LazyStateT   (IndexPosition FilePath) m)))
  ----specForward @(Fwd (FileT (Forward text) (IdentityT (StrictStateT (Position () FilePath)   m))))
  ----specForward @(Fwd (FileT (Forward text) (IdentityT (StrictStateT (IndexPosition FilePath) m))))

  ----specForward @(Fwd (FileT (Forward text) (CPSRWST    () () (Position () FilePath)   m)))
  ----specForward @(Fwd (FileT (Forward text) (CPSRWST    () () (IndexPosition FilePath) m)))
  specForward @(Fwd (FileT (Forward text) (LazyRWST   r w s   m)))
  --specForward @(Fwd (FileT (Forward text) (LazyRWST   () () (IndexPosition FilePath) m)))
  ----specForward @(Fwd (FileT (Forward text) (StrictRWST () () (Position () FilePath)   m)))
  ----specForward @(Fwd (FileT (Forward text) (StrictRWST () () (IndexPosition FilePath) m)))

specForwardsPure :: forall text m r w s char.
  ( Element text ~ char
  , OpenFrom text
  , Typeable text
  , Show text
  , Eq text
  , IsString text
  , IsSequence text
  , MonadFail m
  , ShouldReturn (BaseMonad m)
  , Typeable m
  , RunBase (TestParameters 'Forward (Read m) (StateSeq s text) String) m
  , RunBase (TestParameters 'Forward r (StateSeq s text) String) m
  , forall a. Show a => ShowStM' m a
  , forall a. Eq a => EqStM' m a
  , Show char
  , Eq char
  , IsChar char
  , Show s
  , Eq s
  , ConstructParameter String (TestParameters 'Forward r (StateSeq s text) String)
  , Typeable s
  , UpdateStateWithElement s char
  , Show w
  , Eq w
  , Typeable w
  , Monoid w
  , Typeable r
  , ConstructParameter String (TestParameters 'Forward (Read m) (StateSeq s text) String)
  , ShouldFail (BaseMonad m)
  , forall v. MakeForwardResult m v (v, StateSeq s text)
  , forall v. MakeForwardResult m v ((v, w), StateSeq s text)
  , forall v. MakeForwardResult m v (v, StateSeq s text, w)
  , Peek m
  ) => Spec
specForwardsPure = do
  specForward @(Fwd (LazyStateT (StateSeq s text)   m))
  specForward @(Fwd (LazyWriterT w (LazyStateT (StateSeq s text)   m)))
  --specForward @(Fwd (StrictStateT (StateSeq s text) m))
  --specForward @(Fwd (CPSRWST    () () (StateSeq (Position () FilePath)   text) m))
  specForward @(Fwd (LazyRWST   r w (StateSeq s text)   m))
  --specForward @(Fwd (StrictRWST () () (StateSeq s   text) m))

specForward :: forall (p :: Type -> Type -> Type) r s char.
  ( One char p
  , Profunctor p
  , forall u. ShouldReturnQ p u
  , forall u. Applicative (p u)
  , forall u. Peek (p u)
  , Typeable p
  , forall u a. Show a => ShowStM' (p u) a
  , forall u a. Eq a => EqStM' (p u) a
  , forall u v. MakeForwardResult (p u) v v
  , forall u. RunBase (TestParameters 'Forward r s String) (p u)
  , ConstructParameter String (TestParameters 'Forward r s String)
  , r ~ Read (p ())
  , s ~ State (p ())
  , Show char
  , Eq char
  , IsChar char
  , forall u. ShouldFailQ p u
  ) => Spec
specForward = describe (show $ typeRep @p) do
  describe "one" do
    let f :: FilePath -> String -> BaseMonad (Biparser p char) (StM' (Biparser p char) char)
        f fp str = run @'Forward @p @r @s oneBP fp str
    it "success" let
      fp = "one-success-forward.test"
      in f fp "abc" `shouldReturn` makeResult
          (Position @() fp 1 2)
          (IndexPosition fp 1)
          "bc"
          (fromChar @char 'a')
    it "none to take" let
      fp = "one-none-to-take-forward.test"
      in shouldFail $ f fp mempty
  describe "peek" do
    describe "peek one" do
      let f = run @'Forward @p @r @s peekOneBP
      it "success" let
        fp = "peek-one-success-forward.test"
        in f fp "abc" `shouldReturn` makeResult
            (Position @() fp 1 1)
            (IndexPosition fp 0)
            "abc"
            (fromChar @char 'a')
      it "fail" let
        fp = "peek-one-fail-forward.test"
        in shouldFail $ f fp ""
    it "peek tuple" let
      fp = "peek-tuple-forward.test"
      f = run @'Forward @p @r @s peekTupleBP fp
      in f "abc" `shouldReturn` makeResult
          (Position @() fp 1 2)
          (IndexPosition fp 1)
          "bc"
          (fromChar @char 'a', fromChar @char 'a')
--    describe "peek alt" do
--      let f = run @'Forward @p peekAltBP . (() ,)
--      it "take" let
--        fp = "peek-alt-take-forward.test"
--        in f fp "xa" `shouldReturn` makeResult
--            (Position @() fp 1 1)
--            (IndexPosition fp 0)
--            "xa"
--            'x'
--      it "take fail" let
--        fp = "peek-alt-take-fail-forward.test"
--        in f fp "ab" `shouldReturn` makeResult
--            (Position @() fp 1 2)
--            (IndexPosition fp 1)
--            "b"
--            'a'
--      it "no match" let
--        fp = "peek-alt-no-match-forward.test"
--        in shouldFail $ f fp "b"

specBackward :: forall (p :: Type -> Type -> Type) char r s.
  ( One char p
  , Profunctor p
  , forall u. Applicative (p u)
  , forall u. ShouldReturnQ p u
  , forall u v. MakeBackwardResult v (p u) v
  , forall u a. Show a => ShowStM' (p u) a
  , forall u a. Eq a => EqStM' (p u) a
  , Show char
  , IsChar char
  , Eq char
  , forall u. RunBase (TestParameters 'Backward r s u) (p u)
  , forall u. ConstructParameter u r
  , forall u. ConstructParameter u s
  , forall u. Peek (p u)
  , Typeable p
  ) => Spec
specBackward = describe (show $ typeRep @p) do
  describe "one" do
    it "one" let
      fp = "one-backward.test"
      b = run @'Backward @p @r @s oneBP fp
      u = fromChar @char 'a'
      in b u `shouldReturn` makeResult
          "a"
          u
  describe "peek" do
    it "peek one" let
      fp = "peek-one-backward.test"
      b = run @'Backward @p @r @s peekOneBP fp
      u = fromChar @char 'a'
      in b u `shouldReturn` makeResult
        "a"
        u
    it "peek tuple" let
      fp = "peek-tuple-backward.test"
      b = run @'Backward @p @r @s peekTupleBP fp
      u = (fromChar @char 'a', fromChar @char 'b')
      in b u `shouldReturn` makeResult
          "ab"
          u
--
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
--      let bp :: (Peek (p char), Try (p char), Alt (p char), MonadFail (p char), One char p) => Iso p char
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
--    let bp :: (Try (p char), One char p, forall u. MonadFail (p u), Profunctor p) => Iso p char
--        bp = try $ one <* take 'b'
--        f = run @(FM UnixLC Text) bp ()
--        b = runBackward @(BMIO Text) bp ()
--
--    describe "forward" do
--      it "success" $ f "abc" `shouldBe` Right ('a', (Position () 1 3, "c"))
--      
--      it "does not consume state in failed attempt" do
--        run @(FM UnixLC Text) (bp <!> takeUni 'c') () "cde" `shouldBe` Right ('c', (Position () 1 2, "de"))
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
