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
import Lens.Micro
import Lens.Micro.Pro.TH (makePrisms)
import Prelude (IO, print, putStrLn, Show)

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

-- * Run

main :: IO ()
main = do
  abcExample

