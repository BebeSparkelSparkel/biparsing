module Dxf.HeadOfFile where

import Data.String

type EOL a = a

nl :: IsString a => EOL a
nl = "\n"

wnl :: IsString a => EOL a
wnl = "\r\n"

linesHeadOfFile :: IsString a => [a]
linesHeadOfFile =
  [ "  0"
  , "SECTION"
  , "  2"
  , "HEADER"
  , "  9"
  , "$ACADVER"
  , "  1"
  , "AC1027"
  , "  9"
  , "$ACADMAINTVER"
  , " 70"
  , "     8"
  , "  9"
  , "$DWGCODEPAGE"
  , "  3"
  , "ANSI_1252"
  , "  9"
  , "$REQUIREDVERSIONS"
  , "160"
  , "                 0"
  , "  9"
  , "$LASTSAVEDBY"
  , "  1"
  , "williamrusnack"
  , "  9"
  , "$INSBASE"
  , " 10"
  , "0.0"
  , " 20"
  , "0.0"
  , " 30"
  , "0.0"
  , "  9"
  , "$EXTMIN"
  , " 10"
  , "4.910533931955369"
  , " 20"
  , "2.859738596236992"
  , " 30"
  , "0.0"
  ]

parsedHeadOfFile :: IsString a => a -> (EOL a, [(Int, a)])
parsedHeadOfFile eol = (eol ,)
  [ (0, "SECTION")
  , (2, "HEADER")
  , (9, "$ACADVER")
  , (1, "AC1027")
  , (9, "$ACADMAINTVER")
  , (70, "     8")
  , (9, "$DWGCODEPAGE")
  , (3, "ANSI_1252")
  , (9, "$REQUIREDVERSIONS")
  , (160, "                 0")
  , (9, "$LASTSAVEDBY")
  , (1, "williamrusnack")
  , (9, "$INSBASE")
  , (10, "0.0")
  , (20, "0.0")
  , (30, "0.0")
  , (9, "$EXTMIN")
  , (10, "4.910533931955369")
  , (20, "2.859738596236992")
  , (30, "0.0")
  ]

