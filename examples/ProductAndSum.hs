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
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as BC
import Data.Either.Extra (eitherToMaybe)
import Prelude (IO, Int, print, putStrLn, Show)

-- * Product

product :: IsoEasy IndexContext ByteString (ByteString, Int)
product = do
  char '('
  s <- takeWhile (/= fromChar ',') `upon` fst
  char ','
  i <- intBaseTen `upon` snd
  char ')'
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

-- * Run

main :: IO ()
main = do
  productExample
  sumExample

