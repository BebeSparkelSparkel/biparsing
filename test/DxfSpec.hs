{-# OPTIONS_GHC -fno-warn-type-defaults #-}
module DxfSpec where

import Dxf.HeadOfFile
import Test.Hspec
import Control.Monad.Except
import Data.List

default (String)

-- From Biparser Implementation --
type Biparser error from to = ()

parse :: MonadError error m => Biparser error from to -> from -> m to 
parse = error "parse"
-- not sure if mtl should be used

encode :: Biparser error from to -> to -> from
encode = error "encode"

-- Sample Biparsers --

biSplit :: a -> Biparser () a (a, [a])
biSplit = error "biSplit"

data LineError a = LineError Int a deriving (Show, Eq)
biLines :: Biparser (LineError error) a (EOL a, [a])
-- biLines = biSplit "\n" <|> biSplit "\r\n" -- not sure if alternative will actually work here
biLines = error "biLines"

biMany :: Biparser error from to -> Biparser error from [to] -- may need a func
biMany = error "biMany"

biDxfCodeVal :: Biparser String [s] [(Int, s)]
biDxfCodeVal = error "biDxfCodeVal"

type CodeVal s = (EOL s, [(Int, s)])
biCodeVal :: Biparser (LineError String) s (CodeVal s)
-- biCodeVal = biLines . embed _2 (many biDxfCodeVal)
biCodeVal = error "biCodeVal"
-- cool if this were do syntax instead of .

spec :: Spec
spec = do
  describe "convert stream" do
    let nlBs = intercalate nl linesHeadOfFile
    let wnlBs = intercalate wnl linesHeadOfFile

    describe "parse" do
      it "nl" $ parse biCodeVal nlBs `shouldBe` pure @Maybe (parsedHeadOfFile nl)
      it "wnl" $ parse biCodeVal wnlBs `shouldBe` pure @Maybe (parsedHeadOfFile wnl)

    describe "encode" do
      it "nl" $ encode biCodeVal (parsedHeadOfFile nl) `shouldBe` nlBs
      it "wnl" $ encode biCodeVal (parsedHeadOfFile wnl) `shouldBe` wnlBs

    describe "error" do
      it "missing first line" do
        let e :: LineError String
            e = LineError 1 "Expecting an Int"
        let bs = intercalate nl $ tail linesHeadOfFile
        parse biCodeVal bs `shouldBe` (throwError e :: Either (LineError String) (CodeVal String))

      it "missing last line" do
        let ls = init linesHeadOfFile 
        let e :: LineError String
            e = LineError (length ls) "Expecting a value for code 30 but found EOF"
        let bs = intercalate nl ls
        parse biCodeVal bs `shouldBe` (throwError e :: Either (LineError String) (CodeVal String))

