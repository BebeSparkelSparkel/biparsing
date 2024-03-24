{-# OPTIONS_GHC -Wno-orphans #-}
module Biparse.ListSpec where

import Biparse.List
import Data.List.NonEmpty (NonEmpty)
import Data.Text qualified as T

spec :: Spec
spec = do
  fb @() "takeElementsWhile"
    (fmap w2c <$> takeElementsWhile (isDigit . w2c) `upon` fmap c2w :: Iso () IO IO () ByteString () (Identity ByteString) String)
    ()
    ()
    (\f -> do
      it "matches some digits" do
        f "123 abc" >>= (`shouldBe` ("123", " abc"))

      it "matches all characters" do
        f "123" >>= (`shouldBe` ("123", mempty))

      it "matches no digits" do
        f "abc" >>= (`shouldBe` (mempty, "abc"))

      it "empty" do
        f mempty >>= (`shouldBe` (mempty, mempty))
    )
    \b -> do
      it "prints some" $ b "abc" >>= (`shouldBe` ("abc", "abc"))

      it "prints none" $ b mempty >>= (`shouldBe` (mempty, mempty))

  fb @() "takeNElements"
    (takeNElements 2 :: Iso IndexContext (EISP [Int]) EitherString () [Int] () (IndexPosition [Int]) [Int])
    ()
    ()
    (\f -> do
      it "success" do
        f [1,2] `shouldBe` Right ([1,2], IndexPosition 2 mempty)
        f [1,2,3] `shouldBe` Right ([1,2], IndexPosition 2 [3])

      it "fail" do
        f [] `shouldSatisfy` errorIndex 0
        f [1] `shouldSatisfy` errorIndex 1
    )
    \b -> do
      it "success" do
        b [1,2] `shouldBe` EValue ([1,2], [1,2])
        b [1,2,3] `shouldBe` EValue ([1,2], [1,2])

      it "fail" do
        b mempty `shouldSatisfy` isString
        b [1] `shouldSatisfy` isString

  describe "many" do
    fb @() "with takeUni"
      (many $ takeUni 'a' :: Iso () IO IO () Text () (Identity Text) [Char])
      ()
      ()
      (\f -> do
        it "takes none" do
          f mempty >>= (`shouldBe` (mempty, mempty))

        it "takes 2" do
          f "aabc" >>= (`shouldBe` (['a','a'], "bc"))
      )
      \b -> do
        it "prints all" $ b ['a','a'] >>= (`shouldBe` ("aa", "aa"))

        it "prints none" $ b mempty >>= (`shouldBe` (mempty, mempty))

    fb @() "with takeTri"
      (   many
      $   try (takeTri "TRUE" True 1)
      <!>      takeTri "FALSE" False 0
      :: Biparser () (Identity [String]) Maybe Maybe () [String] () [Bool] [Int])
      ()
      ()
      (\f -> do
        it "takes two" do
          f ["TRUE","FALSE","UNDEFINED"]
            `shouldBe` Just ([1,0],["UNDEFINED"])

        it "takes none" do
          f ["UNDEFINED"]
            `shouldBe` Just (mempty, ["UNDEFINED"])
          f mempty
            `shouldBe` Just (mempty, mempty)
      )
      \b -> do
        it "prints all" do
          b [False, True]
            `shouldBe` Just ([0,1], ["FALSE", "TRUE"])

        it "prints none" do
          b mempty
            `shouldBe` Just (mempty, mempty)

  fb @() "some"
    (some (takeUni 1) :: Iso () IO IO () [Int] () (Identity [Int]) (NonEmpty Int))
    ()
    ()
    (\f -> do
      it "fails on none" do
        f mempty `shouldThrow` isUserError
        f [2] `shouldThrow` isUserError

      it "takes some" do
        f [1]     >>= (`shouldBe` ([1],   mempty))
        f [1,2]   >>= (`shouldBe` ([1],   [2]))
        f [1,1,2] >>= (`shouldBe` ([1,1], [2]))
    )
    \b -> do
      it "prints all" $ b [1,1,1] >>= (`shouldBe` ([1,1,1], [1,1,1]))

  fb @() "all"
    (all $ takeUni 'a' <!> takeUni 'b' :: Iso UnixLC (FM Text) IO () Text () (Position () Text) [Char])
    ()
    ()
    (\f -> do
      it "empty" do
        f "" `shouldBe` Right (mempty, "")

      it "one" $ f "a" `shouldBe` Right ("a", Position () 1 2 mempty)

      it "two" $ f "ba" `shouldBe` Right ("ba", Position () 1 3 mempty)

      describe "fail" do
        it "no matches" $ f "c"   `shouldSatisfy` errorPosition 1 1

        it "some matches" $ f "abc" `shouldSatisfy` errorPosition 1 3
    )
    \b -> do
      it "empty" $ b mempty >>= (`shouldBe` (mempty,mempty))

      it "one" $ b "a" >>= (`shouldBe` ("a", "a"))

      it "two" $ b "ab" >>= (`shouldBe` ("ab", "ab"))

      it "fail" do
        b "c" `shouldThrow` isUserError
        b "abc" `shouldThrow` isUserError

  describe "splitElem" do
    let bp :: Iso () IO IO () Text () (Identity Text) [Text]
        bp = splitElem ':'
        f = runForward @() bp
        b = runBackward bp () ()
        t name f' b' = describe name do
          it "forward" $ limit do
            f (Identity f') >>= (`shouldBe` (b', mempty))
          it "backward" $ limit do
            y <- b b'
            y `shouldBe` (b', f')

    t "empty" mempty mempty
    t "one element no splits" "a" ["a"]
    t "only split element" ":" [mempty,mempty]
    t "triple split" ":a:bc" [mempty,"a","bc"]
    t "empty last" "ab:" ["ab",mempty]
    t "empty first" ":ab" [mempty,"ab"]

    let ef = evalForward @() bp
    prop "forward should never return [\"\"]" $ forAll (Identity . T.pack <$> listOf (elements "ab:")) \string -> do
      ef string >>= (`shouldNotBe` [mempty])

  fb @() "splitOn"
    (splitOn "ab" :: Iso () IO IO () Text () (Identity Text) [Text])
    ()
    ()
    (\f -> do
      it "empty" $ limit do
        f mempty >>= (`shouldBe` (mempty,mempty))

      it "no split" $ limit do
        f "cde" >>= (`shouldBe` (["cde"], mempty))

      it "match start" $ limit do
        f "abcd" >>= (`shouldBe` (["", "cd"], mempty))

      it "match start two" $ limit do
        f "abcdabef" >>= (`shouldBe` (["", "cd", "ef"], mempty))

      it "match end" $ limit do
        f "cab" >>= (`shouldBe` (["c", ""], mempty))

      it "splits some" $ limit do
        f "abcdababefab" >>= (`shouldBe` (["", "cd", "", "ef", ""], mempty))
    )
    \b -> do
      it "empty" $ limit $ b mempty >>= (`shouldBe` (mempty, mempty))

      it "one" $ limit $ b ["cd"] >>= (`shouldBe` (["cd"], "cd"))

      it "two" $ limit $ b ["cd", "ef"] >>= (`shouldBe` (["cd", "ef"], "cdabef"))

      it "some empty" $ limit do
        let xs = ["", "cd", "", "ef", ""] :: [Text]
        x <- b xs
        x `shouldBe` (xs,"abcdababefab")

  fb @() "whileM"
    (whileM (peek (memptyWrite one >>= \x -> pure $ x /= 'x')) one :: Iso () IO IO () Text () (Identity Text) [Char])
    ()
    ()
    (\f -> do
      it "empty" do
        f mempty >>= (`shouldBe` (mempty,mempty))

      it "takes all" do
        f "abc" >>= (`shouldBe` ("abc",mempty))

      it "takes none" do
        f "x" >>= (`shouldBe` (mempty,"x"))

      it "takes till x" do
        f "abxc" >>= (`shouldBe` ("ab","xc"))
    )
    \b -> do
      it "empty" $ b mempty >>= (`shouldBe` (mempty,mempty))

      it "prints all" $ b "abc" >>= (`shouldBe` ("abc","abc"))

      it "prints till x" $ b "abxc" >>= (`shouldBe` ("ab","ab"))

  --describe "whileId" do
  --  describe "THIS IS WACK" do
  --    let o = one :: Iso () Maybe Maybe (Identity Text) Char
  --        m2i x = Identity . fromMaybe (False,x)
  --        bp :: Iso () Identity Identity (Identity Text) [Maybe Char]
  --        bp = whileId
  --          ( memptyWrite $ mapMs' m2i m2i $ comap (fromMaybe '0') $ peek do
  --            x <- o
  --            pure $ x /= 'x'
  --          )
  --          (fmap pure $ mapMs (pure . fromJust) (pure . fromJust) $ (o `uponMay` '0') id)
  --
  --    describe "forward" do
  --      let f = runForward bp
  --
  --      it "empty" do
  --        f mempty `shouldBe` Identity (mempty,mempty)
  --
  --      it "takes all" do
  --        f "abc" `shouldBe` Identity ("abc",mempty)
  --
  --      it "takes none" do
  --        f "x" `shouldBe` Identity (mempty,"x")
  --
  --      it "takes till x" do
  --        f "abxc" `shouldBe` Identity ("ab","xc")
  --
  --    describe "backward" do
  --      let b = runBackward bp
  --
  --      it "empty" do
  --        b mempty `shouldBe` Identity (mempty,mempty)
  --
  --      it "prints all" do
  --        b "abc" `shouldBe` Identity ("abc","abc")
  --
  --      it "prints till x" do
  --        b "abxc" `shouldBe` Identity ("ab","ab")

  --describe "untilId" do
  --  let bp :: Iso () Identity Identity (Identity String) [String]
  --      bp = untilId isNull bp'
  --      bp' :: Iso () Identity Identity (Identity String) String
  --      bp' = split do
  --        x <- get
  --        maybe undefined (\(y,z) -> y <$ put z) $
  --          liftA2 (,) (pure <$> headMay x) (tailMay x)

  --  describe "forward" do
  --    let f = runForward bp

  --    it "mempty" $ f mempty `shouldBe` Identity (mempty,mempty)

  --    it "takes all" $ f "ab" `shouldBe` Identity (["a","b"],mempty)

  --  describe "backward" do
  --    let b = runBackward bp

  --    it "mempty" $ b mempty `shouldBe` Identity (mempty,mempty)

  --    it "takes all" $ b ["a","b"] `shouldBe` Identity (["a","b"],"ab")

  fb @() "whileFwdAllBwd"
    (whileFwdAllBwd (take 3 $> False <!> pure True) one :: Iso () EitherString EitherString () [Int] () (Identity [Int]) [Int])
    ()
    ()
    (\f -> do
      it "empty success" $ f [3] `shouldBe` EValue (mempty,mempty)
      it "some success" $ f [0,1,2,3,4] `shouldBe` EValue ([0,1,2],[4])
      it "fail not predicate success" $ f [0,1,2] `shouldSatisfy` isString
    )
    \b -> do
      it "empty u" $ b [] `shouldBe` EValue ([],[3])
      it "some u" $ b [0,1,2] `shouldBe` EValue ([0,1,2],[0,1,2,3])

  fb @() "untilFwdSuccessBwdAll"
    (one `untilFwdSuccessBwdAll` take 3 :: Iso () EitherString EitherString () [Int] () (Identity [Int]) [Int])
    ()
    ()
    (\f -> do
      it "empty success" $ f [3] `shouldBe` EValue (mempty,mempty)
      it "some success" $ f [0,1,2,3,4] `shouldBe` EValue ([0,1,2],[4])
      it "fail not predicate success" $ f [0,1,2] `shouldSatisfy` isString
    )
    \b -> do
      it "empty u" $ b [] `shouldBe` EValue ([],[3])
      it "some u" $ b [0,1,2] `shouldBe` EValue ([0,1,2],[0,1,2,3])

  fb @() "untilInclusive"
    (untilInclusive (== 'c') one :: Iso UnixLC (FM (Seq Char)) IO () (Seq Char) () (Position () (Seq Char)) String)
    ()
    ()
    (\f -> do
      it "empty" $ f "" `shouldSatisfy` errorPosition 1 1
      it "no match" $ f "ab" `shouldSatisfy` errorPosition 1 3
      it "only match" $ f "c" `shouldBe` Right (['c'], Position () 1 2 "")
      it "first match" $ f "cab" `shouldBe` Right (['c'], Position () 1 2 "ab")
      it "middle match" $ f "abcd" `shouldBe` Right ("abc", Position () 1 4 "d")
      it "last match" $ f "abc" `shouldBe` Right ("abc", Position () 1 4 "")
      it "middle match" $ f "abcd" `shouldBe` Right ("abc", Position () 1 4 "d")
    )
    \b -> do
      it "empty" $ b "" `shouldThrow` isUserError
      it "no match" $ b "ab" `shouldThrow` isUserError
      it "middle match" $ b "abcd" >>= (`shouldBe` ("abc","abc"))

  fb @() "intersperse"
    (intersperse (one >>= \x -> if x == 'x' then pure x else fail "not x")
      (one `upon` const ',' >>= \x -> if x == ',' then pure x else fail "not ,")
      :: Iso ColumnsOnly (FM String) EitherString () String () (Position () String) [Char])
    ()
    ()
    (\f -> do
      it "empty" $ f "" `shouldBe` Right ([], Position () 1 1 "")
      it "parse none" $ f "y" `shouldBe` Right ([], Position () 1 1 "y")
      it "single" $ f "x" `shouldBe` Right ("x", Position () 1 2 "")
      it "single with fail on seperator" $ f "xy" `shouldBe` Right ("x", Position () 1 2 "y")
      it "single with seperator" $ f "x," `shouldBe` Right ("x", Position () 1 2 ",")
      it "double" $ f "x,x" `shouldBe` Right ("xx", Position () 1 4 "")
      it "double with fail on seperator" $ f "x,xy" `shouldBe` Right ("xx", Position () 1 4 "y")
    )
    \b -> do
      it "empty" $ b "" `shouldBe` EValue ("", "")
      it "single" $ b "x" `shouldBe` EValue ("x", "x")
      it "double" $ b "xx" `shouldBe` EValue ("xx", "x,x")

instance {-# OVERLAPS #-} IsString [Maybe Char] where fromString = fmap pure

