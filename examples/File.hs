{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE Unsafe #-}
import Biparse.File
import Biparse.Mixes.Exports
import Control.Monad
import Data.Bool qualified
import Data.List qualified  as L
import System.Exit (exitFailure)
import System.IO (IO, readFile, writeFile, print, hPutStr, stderr)

data Ini = Ini
  { iniGlobals :: [(String, String)]
  , iniSections :: [(String, [(String, String)])]
  } deriving (Show)

ini :: IsoEasy String IO Ini
ini = do
  iniGlobals <- keyValues `upon` iniGlobals
  iniSections <- comap iniSections $ many do
    skipNonData
    section
  return Ini {..}

section :: IsoEasy String IO (String, [(String, String)])
section = (,) <$> sectionName `upon` fst <*> keyValues `upon` snd

sectionName :: IsoEasy String IO String
sectionName = char '[' *> breakAt ']' <* dropUntil '\n'

keyValues :: IsoEasy String IO [(String, String)]
keyValues = many keyValue

keyValue :: IsoEasy String IO (String, String)
keyValue = do
  skipNonData
  shouldFail sectionName "Found a section header not a key value"

  key <- breakAt '=' `upon` fst
  value <- breakWhen (char '\n') `upon` snd
  return (key, value)

skipNonData :: ConstEasy String IO u
skipNonData = drop (comment <!> emptyLine)

comment :: ConstEasy String IO u
comment = char '#' *> dropUntil '\n'

emptyLine :: ConstEasy String IO u
emptyLine = char '\n'

main :: IO ()
main = do
  let filePath = "/tmp/biparser-file-example.ini"
  writeFile filePath sample
  iniData <- decodeFileEasy filePath ini
  print iniData
  void $ encodeToFileEasy "/tmp/biparser-file-example.ini" iniData ini

  -- make it a test
  let stripped = L.unlines . L.filter (\x -> Data.Bool.not (L.null x) && L.head x /= '#') . L.lines $ sample
  result <- readFile filePath
  unless (result == stripped) do
    hPutStr stderr $ "\nERROR: printed wrong data to " <> filePath <> "\nshould be\n```\n" <> stripped <> "\n```\n\nbut is\n\n```\n" <> result <> "\n```\n"
    exitFailure

sample :: String
sample =
  "Application=WebServer\n\
  \\n\
  \[Settings]\n\
  \\n\
  \#======================================================================\n\
  \\n\
  \# Set detailed log for additional debugging info\n\
  \\n\
  \DetailedLog=1\n\
  \\n\
  \RunStatus=1\n\
  \\n\
  \StatusPort=6090\n\
  \\n\
  \StatusRefresh=10\n\
  \\n\
  \Archive=1\n\
  \\n\
  \# Sets the location of the MV_FTP log file\n\
  \\n\
  \LogFile=/opt/ecs/mvuser/MV_IPTel/log/MV_IPTel.log\n\
  \\n\
  \#======================================================================\n\
  \\n\
  \Version=0.9 Build 4 Created July 11 2004 14:00\n\
  \\n\
  \ServerName=Unknown\n"

