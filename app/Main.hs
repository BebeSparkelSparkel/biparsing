module Main where

import GHC.Generics
import Biparse
import Control.Monad ( (>=>), (<=<) )
import Control.Monad.Trans.State.Lazy( StateT, evalStateT, get )
import Data.Bifunctor ( bimap )
import Data.Functor.Barbie ( FunctorB(bmap) )
import Data.Functor.Identity ( Identity )
import Lens.Micro
import Prelude as P
import System.Environment (getArgs)
import System.Exit (exitWith, ExitCode( ExitFailure) )
import Data.Monoid.Generic ( GenericSemigroup, GenericMonoid )
import Data.Bifunctor( Bifunctor(first) )

lines :: Iso String [String]
lines = Biparser P.lines unlines

type LineNumber = Int
type NumberedLine = (LineNumber, String)

number :: Biparser ([String] -> [NumberedLine]) ([String] -> [String])
number = undefined

data GroupCodeValueType a = GroupCodeValueType Int a

groupCodeValueTypes :: Biparser ([NumberedLine] -> [(LineNumber, GroupCodeValueType String)]) ([GroupCodeValueType String] -> [String])
groupCodeValueTypes = Biparser parse serialize
  where
  parse =

data DXF h c t b e o m = DXF
  { header :: m h
  , classes :: m c
  , tables :: m t
  , blocks :: m b
  , entities :: m e
  , objects :: m o
  }
  -- deriving Generic
  -- deriving Semigroup via GenericSemigroup (DXF h c t b e o m)
  -- deriving Monoid via GenericMonoid (DXF h c t b e o m)

instance 
  ( Semigroup (m h)
  , Semigroup (m c)
  , Semigroup (m t)
  , Semigroup (m b)
  , Semigroup (m e)
  , Semigroup (m o) ) =>
  Semigroup (DXF h c t b e o m) where
  x <> y = undefined

instance 
  ( Monoid (m h)
  , Monoid (m c)
  , Monoid (m t)
  , Monoid (m b)
  , Monoid (m e)
  , Monoid (m o) ) =>
  Monoid (DXF h c t b e o m) where
  mempty = DXF mempty mempty mempty mempty mempty mempty

data Section
  = Header
  | Classes
  | Tables
  | Blocks
  | Entities
  | Objects

sectionFromString :: String -> Maybe Section
sectionFromString = undefined

data Result a
  = Error (Maybe LineNumber) String
  | ErrorMultiple [(Maybe LineNumber, String)]
  | Success a
  deriving (Generic, Functor)

instance Applicative Result where
  pure = Success
  (<*>) = undefined

instance Monad Result where
  (>>=) = undefined


section :: Biparser (StateT [NumberedLine] Result (Section, [NumberedLine])) ([NumberedLine] -> [NumberedLine])
section = Biparser parser id
  where
  parser = undefined
  -- parser = do
  --   get >>= \ls -> case ls of
  --     (_, "  0") : (_, "SECTION") : (_, "  2") :  -> case ls' of

type RawSections m = DXF [NumberedLine] [NumberedLine] [NumberedLine] [NumberedLine] [NumberedLine] [NumberedLine] m

sections :: Biparser ([NumberedLine] -> Result (RawSections Identity)) ((RawSections Identity) -> [NumberedLine])
sections = 
  first (\f -> errorIfNotAllSectionsFound <=< evalStateT (f mempty))
  ( addSection header <=>>
    addSection classes <=>>
    addSection tables <=>>
    addSection blocks <=>>
    addSection entities <=>>
    addSection objects ) 
  where
  setterBySection :: Section -> NumberedLine -> RawSections a -> RawSections a
  setterBySection = undefined

  getterBySection :: Section -> RawSections a -> a [NumberedLine]
  getterBySection = undefined

  addSection
    :: (RawSections a -> a [NumberedLine])
    -> Biparser
        (RawSections Maybe -> StateT [NumberedLine] Result (RawSections Maybe))
        (RawSections Identity -> [NumberedLine])
  addSection getter = undefined
  -- addSection s nls = flip evalStateT nls do
  --   (section, sectionLines) <- s
  --   case section of
  --     Header

  (<=>>) :: (Semigroup c, Monad m) => Biparser (a -> m a) (b -> c) -> Biparser (a -> m a) (b -> c) -> Biparser (a -> m a) (b -> c)
  Biparser x y <=>> Biparser x' y' = Biparser (x >=> x') \z -> y z <> y' z

  errorIfNotAllSectionsFound :: RawSections Maybe -> Result (RawSections Identity)
  errorIfNotAllSectionsFound = undefined
    -- subSequence header (\x y -> x { header = y }) "Missing the HEADER section" >=>
    -- subSequence classes (\x y -> x { classes = y }) "Missing the CLASSES section" >=>
    -- subSequence tables (\x y -> x { tables = y }) "Missing the TABLES section" >=>
    -- subSequence blocks (\x y -> x { blocks = y }) "Missing the BLOCKS section" >=>
    -- subSequence entities (\x y -> x { entities = y }) "Missing the ENTITIES section" >=>
    -- subSequence objects (\x y -> x { objects = y }) "Missing the OBJECTS section"
    -- where
    -- subSequence :: (a -> m b) -> (a -> n b -> c) -> String -> a -> Result c
    -- subSequence = undefined

pheader :: Biparser (DXF h c t b e o m -> Result (DXF h c t b e o m)
pheader 


main :: IO ()
main = getArgs >>= pure . (^? ix 0) >>= \case
    Nothing -> do
      print "Error: Need DXF file argument."
      exitWith $ ExitFailure 1
    Just fileName -> do
      contents <- readFile fileName
      putStr contents
