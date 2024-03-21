{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternSynonyms #-}
module Control.Monad.EitherString
  ( EitherString(EString, EValue, ..)
  , _EString
  , _EValue
  , getString
  , getValue
  , isString
  , eitherString
  ) where

import Control.Applicative (Applicative, Alternative((<|>),empty))
import Control.Lens (Prism, Prism', prism, prism')
import Control.Monad (Monad, MonadFail(fail), MonadPlus)
import Control.Monad.Except (MonadError)
import Data.Bool (Bool)
import Data.Coerce (coerce)
import Data.Either (Either(Left,Right), fromLeft, fromRight, isLeft, either)
import Data.Eq (Eq)
import Data.Function ((.), ($))
import Data.Functor (Functor)
import Data.Functor.Alt (Alt, (<!>))
import Data.Maybe (Maybe(Just,Nothing))
import Data.Monoid ((<>))
import Data.Ord (Ord)
import Data.String (String)
import Text.Show (Show, show)

newtype EitherString a = EitherString {runEitherString :: Either String a} deriving (Eq, Ord, Functor, Applicative, Monad)

-- _Left :: Prism (Either a c) (Either b c) a b
_EString :: Prism' (EitherString a) String
_EString = prism' EString \case
  EString x -> Just x
  _ -> Nothing

_EValue :: Prism (EitherString a) (EitherString b) a b
_EValue = prism EValue \case
  EValue x -> Right x
  EString x -> Left $ EString x

instance Show a => Show (EitherString a) where
  show = \case
    EitherString (Right x) -> "EValue (" <> show x <> ")"
    EitherString (Left x) -> "EString " <> show x

instance Alt EitherString where EitherString x <!> EitherString y = EitherString $ x <!> y

pattern EString :: String -> EitherString a
pattern EString x = EitherString (Left x)

pattern EValue :: a -> EitherString a
pattern EValue x = EitherString (Right x)

{-# COMPLETE EString, EValue #-}

instance Alternative EitherString where
  empty = EitherString $ Left empty
  x@(EitherString (Right _)) <|> _ = x
  _ <|> x = x

instance MonadFail EitherString where
  fail = EitherString . Left

deriving instance MonadError String EitherString

instance MonadPlus EitherString

getString :: String -> EitherString a -> String
getString x = fromLeft x . runEitherString

getValue :: a -> EitherString a -> a
getValue x = fromRight x . runEitherString

isString :: EitherString a -> Bool
isString = isLeft . runEitherString

eitherString :: (String -> b) -> (a -> b) -> EitherString a -> b
eitherString f g = coerce $ either f g

