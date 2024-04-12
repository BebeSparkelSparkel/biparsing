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
import Biparse.Mixes.Either
import Control.Monad.EitherString (EitherString)
import Data.Sequences (replicate)
import Data.Text (Text)
import Data.Text qualified as T
import Lens.Micro
import Lens.Micro.Pro.TH (makePrisms)
import Prelude (IO, Int, print, putStrLn, Show)
import Biparse.Biparser.StateReaderWriter qualified as SRW
import Biparse.Biparser qualified as BB

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
indent (BB.Biparser fw bw) = zoomOne @(Mixes ElementToList) @(Mixes ColumnsOnly) @Either @[Text] $ bp
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
  polylineExample

