{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Biparse.BNF.Tokenize.Type
  ( Token
  , TokenT
  , Token'(..)
  ) where

import Numeric.Natural (Natural)
import Data.Kind (Type)
import Data.Data (Data)
import GHC.Generics (Generic)
import Text.Show (Show)
import Data.Eq (Eq)
import Data.Ord (Ord)

type Token text = Token' text Natural

type TokenT = Token' Type Type

data Token' text count
  = Less -- <
  | Greater -- >
  | Name text
  | Assignment -- ::=
  | Alt -- |
  | OP -- (
  | CP -- )
  -- | Count count -- Natural Number
  | Star -- *
  | OB -- [
  | CB -- ]
  | Pound -- #
  | Comment text -- ; ...
  | Spaces count
  | Tabs count
  | Newlines count
  | DoubleQuote -- "
  deriving (Show, Eq, Ord, Data, Generic)

