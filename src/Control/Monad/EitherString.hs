{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternSynonyms #-}
module Control.Monad.EitherString
  ( EitherString(EString, EValue, ..)
  , getString
  , getValue
  , isString
  ) where

import Data.Bool (Bool)
import Data.Either (Either(Left,Right), fromLeft, fromRight, isLeft)
import Data.String (String)
import Text.Show (Show)
import Data.Ord (Ord)
import Data.Eq (Eq)
import Data.Functor (Functor)
import Data.Function ((.), ($))
import Control.Applicative (Applicative, Alternative((<|>),empty))
import Control.Monad (Monad, MonadFail(fail), MonadPlus)

newtype EitherString a = EitherString {runEitherString :: Either String a} deriving (Show, Eq, Ord, Functor, Applicative, Monad)

pattern EString :: String -> EitherString a
pattern EString x = EitherString (Left x)

pattern EValue :: a -> EitherString a
pattern EValue x = EitherString (Right x)

instance Alternative EitherString where
  empty = EitherString $ Left empty
  x@(EitherString (Right _)) <|> _ = x
  _ <|> x = x

instance MonadFail EitherString where
  fail = EitherString . Left

instance MonadPlus EitherString

getString :: String -> EitherString a -> String
getString x = fromLeft x . runEitherString

getValue :: a -> EitherString a -> a
getValue x = fromRight x . runEitherString

isString :: EitherString a -> Bool
isString = isLeft . runEitherString

