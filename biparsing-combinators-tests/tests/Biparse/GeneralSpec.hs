{-# LANGUAGE AllowAmbiguousTypes #-}
module Biparse.GeneralSpec where

spec :: Spec
spec = runAllTests @() @() @() @() @() @() @TestSuite testSuite


type TestSuite :: (Type -> Type -> Type) -> Constraint
class TestSuite p where testSuite :: Proxy p -> Spec
instance
  ( forall u. RunBase (TestParameters r s u String) (p u)
  , forall u v. MakeResultQ direction p u v
  , forall u. ShouldReturnQ p u
  , forall u. ShouldFailQ p u
  , forall u. MonadFail (p u)
  , forall u. Try (p u)
  , forall u a. Show a => ShowStM' (p u) a
  , forall u a. Eq a => EqStM' (p u) a
  , Profunctor p
  , direction ~ WhichDirection (p ())
  , ForwardOnly direction
  , forall u. ConstructParameter u String (TestParameters r s u String)
  , r ~ Read (p ())
  , s ~ State (p ())
  , Typeable p
  , Eq char
  , Show char
  , IsChar char
  , ComapM p m
  , MonadFail m
  , One char (p char)
  , IsString seq
  , StripPrefix seq (p seq)
  ) => TestSuite p where
  testSuite _ = describe (show $ typeRep @p) do
    let run' :: forall u v. Biparser p u v -> FilePath -> u -> String -> BaseMonad (p u) (StM' (p u) v)
        run' = run @p @r @s
    describe "take" do
      describe "single" do
        let f = run' $ take (fromChar 'a' :: char)
        it "matching" let
          fp = "take-single-matching.test"
          in f fp () "abc" `shouldReturn` makeResult @direction
            (Position @() fp 1 2)
            (IndexPosition fp 1)
            "bc"
            "a"
            ()
        forwardOnly @direction $ describe "fail" do
          it "non-matching" $ shouldFail $ f "take-single-fail-non-matching.test" () "bc"
          it "empty" $ shouldFail $ f "take-single-fail-empty.test" () ""
      describe "double" do
        let f = run' $ take (fromChar 'a' :: char) *> take (fromChar 'b' :: char)
        it "matching" let
          fp = "take-double-matching.test"
          in f fp () "abc" `shouldReturn` makeResult @direction
            (Position @() fp 1 3)
            (IndexPosition fp 2)
            "c"
            "ab"
            ()
      describe "takeDi" do
        let f = run' $ takeDi (fromChar 'a' :: char) u
            u = 1 :: Int
        it "match" let
          fp = "takeDi-match.test"
          in f fp u "abc" `shouldReturn` makeResult @direction
            (OnePos fp)
            "bc"
            "a"
            u
        it "no-match" $ shouldFail $ f "takeDi-no-match.test" 2 "bc"
      describe "takeTri" do
        let f = run' $ takeTri (fromChar 'a' :: char) u v
            u = 1 :: Int
            v = 'A'
        it "match" let
          fp = "takeTri-match.test"
          in f fp u "abc" `shouldReturn` makeResult @direction
            (OnePos fp)
            "bc"
            "a"
            v
        it "no-match" $ shouldFail $ f "takeTri-no-match.test" 2 "bc"
      describe "takeNot" do
        let f = run' $ takeNot (fromChar 'a' :: char)
        it "no-match" let
          fp = "takeNot-no-match.test"
          u = fromChar 'z' :: char
          in f fp u "zabc" `shouldReturn` makeResult @direction
            (OnePos fp)
            "abc"
            "z"
            u
        it "match" $ shouldFail $ f "takeNot-match-test" (fromChar 'a') "abc"
      describe "takeDi'" do
        let f = run' $ takeDi' (fromString "abc" :: seq) u
            u = 1 :: Int
        it "match" let
          fp = "takeDi-prime-match.test"
          in f fp u "abcdef" `shouldReturn` makeResult @direction
            (IndexPos fp 3)
            "def"
            "abc"
            u
      --describe "takeTri'" do

      --describe "takeWhile" do
      --  let f = run' $ takeWhile (/= 'x')
      --  it "empty" let
      --    fp = "takeWhile-empty.test"
      --    u = ""
      --    in f fp "" "" `shouldReturn` makeResult @direction
      --      (ZeroPos fp)
      --      ""
      --      ""
      --      ""
      --  --it "take none" $ f "xab" `shouldBe` Right (mempty, "xab")
      --  --it "take 2" $ f "abx" `shouldBe` Right ("ab", Position () 1 3 "x")
      --  --it "take all" $ f "abc" `shouldBe` Right ("abc", Position () 1 4 mempty)

--    fb @(FM UnixLC Text) @(BMIO Text) "takeWhile"
--      ( let x :: () => Iso p Text
--            x = takeWhile (/= 'x')
--        in (x,x))
--      ()
--      ()
--      (\f -> do
--        it "empty" $ f "" `shouldBe` Right (mempty,"")
--  
--        it "take none" $ f "xab" `shouldBe` Right (mempty, "xab")
--  
--        it "take 2" $ f "abx" `shouldBe` Right ("ab", Position () 1 3 "x")
--  
--        it "take all" $ f "abc" `shouldBe` Right ("abc", Position () 1 4 mempty)
--      )
--      \b -> do
--        it "empty" $ b mempty >>= (`shouldBe` (mempty,mempty))
--  
--        it "no x" $ b "abc" >>= (`shouldBe` ("abc", "abc"))
--  
--        it "has x" $ b "axc" >>= (`shouldBe` ("axc", "axc"))
--  
--    fb @(FM ColumnsOnly (Vector Char)) @(BM (Vector Char)) "drop"
--      ( let x :: () => Biparser p () ()
--            x = drop (take 'a')
--        in (x,x))
--      ()
--      ()
--      (\f -> do
--        it "empty" $ f "" `shouldBe` Right ((), Position () 1 1 "")
--        it "one" $ f "a" `shouldBe` Right ((), Position () 1 2 "")
--        it "many" $ f "aaaa" `shouldBe` Right ((), Position () 1 5 "")
--        it "stops" $ f "aab" `shouldBe` Right ((), Position () 1 3 "b")
--        it "no matches" $ f "bb" `shouldBe` Right ((), Position () 1 1 "bb")
--      )
--      \b -> do
--        it "does not print anything" $ b () `shouldBe` EValue ((), "")
--  
--    fb @(FM LinesOnly (Vector Int)) @(BM (Vector Int)) "dropWhile"
--      ( let x :: () => Iso p ()
--            x = dropWhile (== 1)
--        in (x,x))
--      ()
--      ()
--      (\f -> do
--        it "empty" $ f [] `shouldBe` Right ((), Position () 1 1 [])
--        it "drop none" $ f [2,3] `shouldBe` Right ((), Position () 1 1 [2,3])
--        it "drop all" $ f [1,1] `shouldBe` Right ((), Position () 3 1 [])
--        it "drop some" $ f [1,1,2,3] `shouldBe` Right ((), Position () 3 1 [2,3])
--      )
--      \b -> do
--        it "success" $ b () `shouldBe` EValue ((), mempty)
--  
--    fb @(FM ColumnsOnly (Seq Char)) @(BM (Seq Char)) "dropUntil"
--      ( let x :: () => Iso p ()
--            x = dropUntil 'b'
--        in (x,x))
--      ()
--      ()
--      (\f -> do
--        it "empty" $ f "" `shouldSatisfy` errorPosition 1 1
--        it "not found" $ f "aaa" `shouldSatisfy`errorPosition 1 4
--        it "drop one" $ f "abb" `shouldBe` Right ((), Position () 1 3 "b")
--        it "drop many" $ f "aaab" `shouldBe` Right ((), Position () 1 5 "")
--      )
--      \b -> do
--        it "prints element" $ b () `shouldBe` EValue ((), "b")
--  
--    fb @(FM LinesOnly [Int]) @(BM [Int]) "skipUntil"
--      ( let x :: () => Const p Int
--            x = skipUntil $ (> 2) <$> one
--        in (x,x))
--      ()
--      ()
--      (\f -> do
--        it "empty" $ f [] `shouldSatisfy` errorPosition 1 1
--  
--        it "no true" $ f [0, 1, 2] `shouldSatisfy` errorPosition 4 1
--  
--        it "success" $ f [0, 1, 2, 3, 4] `shouldBe` Right ((), Position () 4 1 [3, 4])
--      )
--      \b -> do
--        it "success" $ b 5 `shouldBe` EValue ((), [5])
--  
--    fb @(FM LinesOnly [Int]) @(BM (Endo [Int])) "untilJust"
--      ( let x :: () => Biparser p Int ()
--            x = untilJust $ bool Nothing (Just ()) . (> 2) <$> one
--        in (x,x))
--      ()
--      ()
--      (\f -> do
--        it "empty" $ f [] `shouldSatisfy` errorPosition 1 1
--  
--        it "no Just" $ f [0, 1, 2] `shouldSatisfy` errorPosition 4 1
--  
--        it "success" $ f [0, 1, 2, 3, 4] `shouldBe` Right ((), Position () 5 1 [4])
--      )
--      \b -> do
--        it "success" $ b 5 `shouldBe` EValue ((), [5])
  
    --fb @(FM UnixLC Text) @(BM (Endo String)) "pad"
    --  ( let x :: () => Iso p Int
    --        x = pad 4 'x' naturalBaseTen
    --    in (x,x))
    --  ()
    --  ()
    --  (\f -> do
    --    it "no pad" do
    --      f "1" `shouldBe` Right (1, Position () 1 2 mempty)
    --      f "123" `shouldBe` Right (123, Position () 1 4 mempty)
  
    --    it "with pad" do
    --      f "x4" `shouldBe` Right (4, Position () 1 3 mempty)
    --      f "xxx456" `shouldBe` Right (456, Position () 1 7 mempty)
  
    --    it "empty fail" do
    --      f "" `shouldSatisfy` errorPosition 1 1
  
    --    describe "only pad" do
    --      it "one" $ f "x" `shouldSatisfy` errorPosition 1 2
    --      it "multiple" $ f "xxx" `shouldSatisfy` errorPosition 1 4
    --  )
    --  \b -> do
    --    it "add pad" do
    --      b 1 >>= (`shouldBe` (1, "xxx1"))
    --      b 123 >>= (`shouldBe` (123, "x123"))
  
    --    it "no pad exact" do
    --      b 1234 >>= (`shouldBe` (1234, "1234"))
  
    --    it "no pad over" do
    --      b 12345 >>= (`shouldBe` (12345, "12345"))
  
    --fb @(FM UnixLC (Seq Char)) @(BM (Endo (Seq Char))) "padSet"
    --  ( let x :: () => Iso p Int
    --        x = padSet 4 'x' ['x','y'] naturalBaseTen
    --    in (x,x))
    --  ()
    --  ()
    --  (\f -> do
    --    it "no pad" do
    --      f "1" `shouldBe` Right (1, Position () 1 2 mempty)
    --      f "123" `shouldBe` Right (123, Position () 1 4 mempty)
  
    --    it "with pad" do
    --      f "x4" `shouldBe` Right (4, Position () 1 3 mempty)
    --      f "xyx456" `shouldBe` Right (456, Position () 1 7 mempty)
  
    --    it "empty fail" do
    --      f "" `shouldSatisfy` errorPosition 1 1
  
    --    describe "only pad" do
    --      it "one" $ f "x" `shouldSatisfy` errorPosition 1 2
    --      it "multiple" $ f "xyx" `shouldSatisfy` errorPosition 1 4
    --  )
    --  \b -> do
    --    it "add pad" do
    --      b 1 >>= (`shouldBe` (1, "xxx1"))
    --      b 123 >>= (`shouldBe` (123, "x123"))
  
    --    it "no pad exact" do
    --      b 1234 >>= (`shouldBe` (1234, "1234"))
  
    --    it "no pad over" do
    --      b 12345 >>= (`shouldBe` (12345, "12345"))
  
--    describe "padCount" do
--      prop "forward" \(NonNegative (na :: Int), NonNegative (nb :: Int), NonNegative (nc :: Int), NonNegative (n :: Int)) -> let
--        as = MT.replicate na 'a'
--        as' = case n - nb of
--          x | x >= 0 -> MT.replicate x 'a'
--            | otherwise -> mempty
--        bs = MT.replicate nb 'b'
--        cs = MT.replicate nc 'c'
--        bp :: Biparser () (Identity (Seq Char)) (FM (Seq Char)) (Either String) () (Seq Char) () (Seq Char) (Natural, Seq Char)
--        bp = padCount n 'a' $ takeWhile (== 'b')
--        f = runForward @() bp
--        b = runBackward bp () () ()
--        n' = fromIntegral n
--        na' = fromIntegral na
--        nb' = fromIntegral nb
--        in do
--          f (Identity $ as <> bs <> cs) `shouldBe` Right ((na' + nb', bs), Identity cs)
--          b bs `shouldBe` Right ((n', bs), as' <> bs)
--  
--    fb @(FM ColumnsOnly (Seq Char)) @(BM (Seq Char -> Seq Char)) "breakWhen"
--      ( let x :: () => Iso p Char
--            x = breakWhen $ stripPrefix "ab"
--        in (x,x))
--      ()
--      ()
--      (\f -> do
--        it "empty" $ f "" `shouldSatisfy` errorPosition 1 1
--  
--        it "break first" do
--          f "abcd" `shouldBe` Right (mempty, Position () 1 3 "cd")
--  
--        it "break last" do
--          f "cdab" `shouldBe` Right ("cd", Position () 1 5 mempty)
--  
--        it "break middle" do
--          f "cdabef" `shouldBe` Right ("cd", Position () 1 5 "ef")
--  
--        it "no break" do
--          f "cdefg" `shouldSatisfy` errorPosition 1 6
--      )
--      \b -> do
--        it "empty" $ b mempty >>= (`shouldBe` (mempty,"ab"))
--  
--        it "append break" $ b "cd" >>= (`shouldBe` ("cd", "cdab"))
--  
--        it "only break" $ b "ab" >>= (`shouldBe` ("ab", "abab"))
--  
--        it "contains break" $ b "cdab" >>= (`shouldBe` ("cdab", "cdabab"))

--  fb @(FM  "rest"
--    ( let x :: () => Iso p Int
--          x = rest
--      in (x,x))
--    ()
--    ()
--    (\f -> do
--      it "success" $ f [1,2,3] `shouldBe` Right ([1,2,3], mempty)
--    )
--    \b -> do
--      it "success" $ b [1,2,3] `shouldBe` EValue ([1,2,3], [1,2,3])
--
--  fb "shouldFail"
--    ( let x :: () => Biparser p () ()
--          x = shouldFail (take 1) "take 1 did not fail"
--      in (x,x))
--    ()
--    ()
--    (\f -> do
--      it "empty" $ f [] `shouldBe` Right ((), Position () 1 1 [])
--      it "take fails" $ f [2] `shouldBe` Right ((), Position () 1 1 [2])
--      it "take succeeds" $ f [1] `shouldSatisfy` errorPosition 1 2
--    )
--    \b -> it "prints nothing" $ b () `shouldBe` EValue ((), [])
--
--  let mapBool :: Bool -> Int
--      mapBool = \case True -> 1; False -> 2
--  fb "optionMaybe"
--    ((,) <$> optionMaybe (takeUni 1 `upon` mapBool $> "one")
--         <*> optionMaybe (takeUni 2 `upon` mapBool $> "two")
--    :: Biparser () (Identity (Vector Int)) IO IO () (Vector Int) () Bool (Maybe String, Maybe String))
--    ()
--    ()
--    (\f -> do
--      it "matches both" do
--        f [1, 2] >>= (`shouldBe` ((Just "one", Just "two"), mempty))
--
--      it "matches first" do
--        f [1, 3] >>= (`shouldBe` ((Just "one", Nothing), [3]))
--
--      it "matches second" do
--        f [2, 3] >>= (`shouldBe` ((Nothing, Just "two"), [3]))
--
--      it "matches none" do
--        f mempty >>= (`shouldBe` ((Nothing, Nothing), mempty))
--    )
--    \b -> do
--      it "prints first" $ b True >>= (`shouldBe` ((Just "one", Nothing), [1]))
--
--      it "prints second" $ b False >>= (`shouldBe` ((Nothing, Just "two"), [2]))
--
--  describe "stripPrefix" do
--    fb "Identity"
--      ( let x :: () => Unit p
--            x = stripPrefix "abc"
--        in (x,x))
--      ()
--      ()
--      (\f -> do
--        it "match" $ f "abcdef" >>= (`shouldBe` ((), "def"))
--
--        it "no match" $ f "def" `shouldThrow` isUserError
--
--        it "empty" $ f "" `shouldThrow` isUserError
--      )
--      \b -> do
--        it "prints prefix" $ b () >>= (`shouldBe` ((), "abc"))
--    
--    fb "LineColumn"
--      ( let x :: () => Unit p
--            x = stripPrefix "abc"
--        in (x,x))
--      ()
--      ()
--      (\f -> do
--        it "match" $ f "abcdef" `shouldBe` Right ((), Position () 1 4 "def")
--
--        it "no match" $ f "def" `shouldSatisfy` errorPosition 1 1
--
--        it "empty" $ f "" `shouldSatisfy` errorPosition 1 1
--      )
--      \b -> do
--        it "prints prefix" $ b () >>= (`shouldBe` ((), "abc"))
--
--  fb "not"
--    ( let x :: () => Biparser p Char Bool
--          x = not $ (== 'x') <$> one
--      in (x,x))
--    ()
--    ()
--    (\f -> do
--      it "true" $ f "ab" >>= (`shouldBe` (True,"b"))
--
--      it "false" $ f "xb" >>= (`shouldBe` (False,"b"))
--    )
--    \b -> do
--      it "true" $ b 'x' >>= (`shouldBe` (False,"x"))
--
--      it "false" $ b 'a' >>= (`shouldBe` (True,"a"))
--
