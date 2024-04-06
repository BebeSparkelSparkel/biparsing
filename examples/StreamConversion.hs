{-| Sometimes it is easier to work on more structured elements than just characaters or bytes.
 - You can use a biparser to convert the data to a different element type and then work on those elements instead.
 - Useful for tokenization.
 -}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilies #-}
import Biparse.Mixes.Either hiding (lines)
import Biparse.Biparser qualified as BB
--import Biparse.General
--import Biparse.Text.Context.LineColumn
--import Control.Monad.State (State)
--import Control.Monad.Writer (Writer)
--import Text.Show (Show, ShowS)
--import Data.Eq (Eq)
--import Data.Int (Int)
--import GHC.Float (Double)
--import Data.String (String)
--import Data.Functor.Alt ((<!>))
--import Control.Monad (return)
--import Data.Tuple (fst, snd)
--import Data.Function (($))
import System.IO (IO, print)
import Data.List (intercalate)

data Type = IntT | DoubleT | StringT deriving (Show, Eq)

data Value = IntV Int | DoubleV Double | StringV String deriving (Show)

linesToTypeStringTuple :: IsoEasy LinesOnly [String] (Type,String)
linesToTypeStringTuple = do
  t <- comap fst
    $   takeDi "int" IntT
    <!> takeDi "double" DoubleT
    <!> pure StringT
  v <- one `upon` snd
  return (t, v)

--lines :: Focus (Mixes UnixLC) (Position d String) [String]
--lines = splitElem '\n'

--stringToTypeStringTuple :: IsoEasy UnixLC String (Type,String)
--stringToTypeStringTuple = focus @() @_ @_ @ lines linesToTypeStringTuple
--
--main :: IO ()
--main = do
--  let textLines :: [String]
--      textLines = 
--        [ "int"
--        , "123"
--        , "double"
--        , "456.789"
--        , "some string"
--        ]
--  let text :: String
--      text = intercalate "\n" textLines
--
--  print $ decodeEasy stringToTypeStringTuple "name of text" text

class ReplaceState m s' m' | m s' -> m', m' -> s' where
  replaceState :: s -> s' -> m' a -> m a

class UpdateStateWithModifiedSubState s ss' s' where
  updateModifiedState :: s -> ss' -> s' -> s

polarize :: forall c' c s s' ss ss' m m' n u v.
  ( ss ~ SubState s
  , ss' ~ SubState s'
  )
  => (s -> s')
  -> Biparser c' s' m' n u v
  -> Biparser c  s  m  n u v
polarize f (BB.Biparser fw bw) = BB.Biparser
  do
    s <- get
    replaceState s (f s) fw
  bw

