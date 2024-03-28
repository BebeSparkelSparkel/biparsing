{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns -Wno-monomorphism-restriction #-}
{-# OPTIONS_GHC -ddump-splices #-}
module Biparse.MixesSpec where

import Language.Haskell.TH
import Biparse.Mixes.IO qualified
import Biparse.Mixes.Either qualified
import Biparse.Mixes.Exports

spec :: Spec
spec = $(
  let
      monads :: [String]
      monads =
        [ "IO"
        , "Either"
        ]
      contexts :: [String]
      contexts =
        [ "IndexContext"
        , "UnixLC"
        , "WindowsLC"
        --, "LinesOnly"
        , "ColumnsOnly"
        , "LineColumnUnknownBreak"
        --, "NoUpdate"
        ]
      subStates :: [String]
      subStates =
        [ "String"
        , "StrictByteString"
        , "LazyByteString"
        , "StrictText"
        , "LazyText"
        ]
  in
  doE (monads    <&> \m  -> noBindS $ varE 'describe `appE` stringE m  `appE`
  doE (contexts  <&> \c  -> noBindS $ varE 'describe `appE` stringE c  `appE`
  doE (subStates <&> \ss -> noBindS $ varE 'describe `appE` stringE ss `appE`
  let 
      cT = conT $ mkName c :: TypeQ
      ssT = conT $ mkName ss :: TypeQ
      decoder = case m of
        "IO" -> [| Biparse.Mixes.IO.decodeEasy |]
        "Either" -> [| Biparse.Mixes.Either.decodeEasy |]
      encoder = case m of
        "IO" -> [| Biparse.Mixes.IO.encodeEasy |]
        "Either" -> [| Biparse.Mixes.Either.encodeEasy |]
  in [| do
          let fw = $decoder @($cT) @($ssT)
              bw = $encoder @($cT) @($ssT)
          describe "one" do
            let bp = one
            it "Forward" $ fw bp def "abc" `shouldReturn` (fromChar 'a')
            it "Backward" $ bw bp (fromChar 'z') `shouldReturn` "z"
          describe "naturalBaseTen" do
            let bp = naturalBaseTen
            it "Forward" $ fw bp def "123" `shouldReturn` (123 :: Word8)
            it "Backward" $ bw bp (123 :: Word8) `shouldReturn` "123"
          describe "multiple state updates" do
            let bp = do
                  take (fromChar '(')
                  x <- one
                  take (fromChar ')')
                  return x
            it "Forward" $ fw bp def "(a)" `shouldReturn` (fromChar 'a')
            it "Backward" $ bw bp (fromChar 'a') `shouldReturn` "(a)"
      |]
  )))
  )
