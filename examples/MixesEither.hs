{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Unsafe #-}
import Biparse.Biparser qualified as BB
import Biparse.Biparser.StateReaderWriter qualified as SRW
import Biparse.Mixes.Either
import Control.Monad.EitherString (EitherString)
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as BC
import Data.Either.Extra (eitherToMaybe)
import Data.Sequences (replicate)
import Data.Text (Text)
import Data.Text qualified as T
import Lens.Micro
import Lens.Micro.Pro.TH (makePrisms)
import Prelude (IO, Int, print, putStrLn, Show)

-- * Product

product :: IsoEasy IndexContext Text (Text, Int)
product = do
  take '('
  s <- takeWhile (/= ',') `upon` fst
  take ','
  i <- intBaseTen `upon` snd
  take ')'
  return (s, i)

productExample :: IO ()
productExample = do
  putStrLn "Product Example"
  print $ decodeEasy product () "(Some text,123)"
  print $ decodeEasy product () "(Some text,)" -- error example
  print $ encodeEasy product ("Some text", 123)

-- * Sum

sum :: IsoEasy (LineColumn 'Unix) ByteString (Either String Int)
sum = right <!> left -- Alt <!> used instead of Alternative <|>
  where
  right = Right <$> intBaseTen `uponMay` eitherToMaybe
  left = Left . BC.unpack <$> rest `upon` \case Left x -> BC.pack x

instance IsChar Word8 where
  fromChar = c2w
  toChar = w2c

sumExample :: IO ()
sumExample = do
  putStrLn "Sum Example"
  print $ decodeEasy sum "some/file/path" "123"
  print $ decodeEasy sum "some/file/path" "abc"
  print $ encodeEasy sum $ Right 123
  print $ encodeEasy sum $ Left "abc"

-- * ADT

data ABC
  = A
  | B Char
  | C Char String
  deriving Show
$(makePrisms ''ABC) -- microlens-pro

abc :: IsoEasy (LineColumn 'Windows) String ABC
abc = a <!> b <!> c
  where
  a = A <$ take 'A' `uponMay` (^? _A)
  b = take 'B' >> B <$> one `uponMay` (^? _B)
  c = comapMay (^? _C) do
    take 'C'
    char' <- one `upon` fst
    str <- rest `upon` snd
    return $ C char' str

abcExample :: IO ()
abcExample = do
  putStrLn "ABC Example"
  print $ decodeEasy abc "/some/file/path" "A"
  print $ decodeEasy abc "/some/file/path" "Bc"
  print $ decodeEasy abc "/some/file/path" "Cdef"
  print $ decodeEasy abc "/some/file/path" "D" -- error example
  print $ encodeEasy abc A
  print $ encodeEasy abc $ B 'c'
  print $ encodeEasy abc $ C 'd' "ef"

-- * Line Parsing

newtype Polyline = Polyline {unPolyline :: [Point]} deriving Show

data IndentType = Space | Tab deriving Show
type IndentCount = Int

data Point
  = PointSimple XY
  | PointDescription Text XY
  deriving Show

data XY = XY {x :: Int, y :: Int} deriving Show

$(makePrisms ''Point)

polyline :: Iso LinesOnly IndentType IndentCount [Text] Polyline
polyline = Polyline <$> many point `upon` unPolyline

point :: Iso LinesOnly IndentType IndentCount [Text] Point
point = pointSimple <!> pointDescription
  where
  pointSimple = PointSimple <$> xy `uponMay` (^? _PointSimple)
  pointDescription = comapMay (^? _PointDescription) do
    take "PointDescription"
    increaseIndent do
      description <- indent rest `upon` fst
      location <- xy `upon` snd
      return $ PointDescription description location

xy :: Iso LinesOnly IndentType IndentCount [Text] XY
xy = do
  indent $ take 'X'
  x <- increaseIndent $ indent $ intBaseTen `upon` x
  indent $ take 'Y'
  y <- increaseIndent $ indent $ intBaseTen `upon` y
  return $ XY x y
  
indent :: forall u v
  .  (forall r ws. SRW.Biparser (Mixes ColumnsOnly) (Position FilePath Text) (FM (Mixes ColumnsOnly) Text) EitherString r Text ws u v)
  -> Biparser LinesOnly [Text] IndentType IndentCount u v
indent (BB.Biparser fw bw) = zoomOne @ElementToList @(Mixes ColumnsOnly) @Either @[Text] $ bp
  where
  bp :: SRW.Biparser (Mixes ColumnsOnly) (Position FilePath Text) (FM (Mixes ColumnsOnly) Text) EitherString IndentType Text IndentCount u v
  bp = BB.Biparser
    do
      s <- get
      let (h, ss') = T.span (`T.elem` " \t") $ getSubState s
      put $ updateSubStateContext @ColumnsOnly s h ss'
      fw
    \u -> do
      c <- asks \case Space -> ' '; Tab -> '\t'
      n <- get
      tell $ (replicate n c :: Text)
      bw u

increaseIndent
  :: Biparser LinesOnly [Text] IndentType IndentCount u v
  -> Biparser LinesOnly [Text] IndentType IndentCount u v
increaseIndent bp = do
  onlyBackwards $ const $ modify succ
  x <- bp
  onlyBackwards $ const $ modify pred
  return x

polylineExample :: IO ()
polylineExample = do
  putStrLn "Polyline Example"
  print $ decode polyline "/some/file/path"
    [ "X"
    , "\t123"
    , "Y"
    , "\t456"
    , "PointDescription"
    , "\ta very special point"
    , "\tX"
    , "\t\t789"
    , "\tY"
    , "\t\t123"
    ]
  print $ encode polyline Tab 0 $ Polyline
    [ PointSimple $ XY 123 456
    , PointDescription "a very special point" $ XY 789 123
    ]

-- * Run

main :: IO ()
main = do
  productExample
  sumExample
  abcExample
  polylineExample

