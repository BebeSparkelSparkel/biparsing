-- | Concrete Syntax Tree
-- Reference: https://stackoverflow.com/questions/1888854/what-is-the-difference-between-an-abstract-syntax-tree-and-a-concrete-syntax-tre
module Biparse.BNF.CST
  ( BNF(..)
  ) where

data BNF
  = Rule
  | Element
  | Alternative
  | LocalAlternative
  | Repetition
  | Optional
  | SpecificRepetition
  | List
  | Comment
  | String
  | WhiteSpace

