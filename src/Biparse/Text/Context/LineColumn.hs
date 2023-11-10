{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Biparse.Text.Context.LineColumn
  ( LineColumn
  , UnixLC
  , WindowsLC
  , LinesOnly
  , ColumnsOnly
  , LineColumnUnknownBreak
  , Position(..)
  , startLineColumn
  , ErrorPosition(..)
  , EEP
  , EESP
  , ElementToList
  , ListToElement
  ) where

import Biparse.Text.LineBreak (LineBreakType(Unix,Windows))
import Biparse.Biparser (SubState, GetSubState(getSubState), UpdateStateWithElement(updateElementContext), UpdateStateWithSubState(updateSubStateContext), ReplaceSubState(replaceSubState))
import Control.Monad.StateError (StateErrorT, ErrorState(ErrorState), ErrorContext, ErrorInstance(ErrorStateInstance), WrapErrorWithState(StateForError,wrapErrorWithState',stateForError), wrapErrorWithState)
import Control.Monad.EitherString (EitherString(EValue,EString))
import GHC.Exts (IsList(Item))
import GHC.Exts qualified as GE
import Control.Monad.ChangeMonad (ChangeMonad(ChangeFunction,changeMonad'), ResultMonad(ResultingMonad,resultMonad))

import GHC.Err (undefined)
import Control.Monad.Trans.Error qualified as E

-- * Contexts

data LineColumn (lineBreak :: LineBreakType)
type UnixLC = LineColumn 'Unix
type WindowsLC = LineColumn 'Windows

data LinesOnly

data ColumnsOnly

data LineColumnUnknownBreak

-- * Postion state

data Position dataId text = Position
  { dataId :: dataId -- | could be FilePath
  , line :: Int
  , column :: Int
  , subState :: text
  } deriving (Show, Eq, Functor)

type instance SubState (LineColumn _)         (Position _ text) = text
type instance SubState LinesOnly              (Position _ text) = text
type instance SubState ColumnsOnly            (Position _ text) = text
type instance SubState LineColumnUnknownBreak (Position _ text) = text

instance GetSubState (LineColumn lb)        (Position dataId text) where getSubState = subState
instance GetSubState LinesOnly              (Position dataId text) where getSubState = subState
instance GetSubState ColumnsOnly            (Position dataId text) where getSubState = subState
instance GetSubState LineColumnUnknownBreak (Position dataId text) where getSubState = subState

type CharCs text char =
  ( Eq char
  , IsChar char
  , char ~ Element text
  )

instance CharCs text char => UpdateStateWithElement (LineColumn 'Unix) (Position dataId text) where
  updateElementContext s@(Position {line,column}) c ss =
    if c == fromChar '\n'
    then s {line = line + 1, column = 1, subState = ss}
    else s {column = column + 1, subState = ss}

instance (CharCs text char, IsSequence text) => UpdateStateWithElement (LineColumn 'Windows) (Position dataId text) where
  updateElementContext s@(Position {line,column}) c ss = case headTailAlt ss of
    Just (c',ss') | c == fromChar '\r' && c' == fromChar '\n' -> s {line = line + 1, column = 1, subState = ss'}
    _ -> s {column = column + 1, subState = ss}

instance (CharCs text char, IsSequence text) => UpdateStateWithElement LineColumnUnknownBreak (Position dataId text) where
  updateElementContext s@(Position {line = l, column = c}) ss ss' = if l == l' && c == c' then w else u
    where
    u@(Position {line = l', column = c'}) = updateElementContext @(LineColumn 'Unix)    s ss ss'
    w = updateElementContext @(LineColumn 'Windows) s ss ss'

instance UpdateStateWithElement LinesOnly (Position dataId [text]) where
  updateElementContext p@(Position {line}) _ ss = p {line = line + 1, column = 1, subState = ss}

instance UpdateStateWithElement ColumnsOnly (Position dataId text) where
  updateElementContext p@(Position {column}) _ ss = p {column = column + 1, subState = ss}

instance (CharCs text char, MonoFoldable text) => UpdateStateWithSubState (LineColumn lb) (Position dataId text) where
  updateSubStateContext = updateSubStateContext @LineColumnUnknownBreak

instance (CharCs text char, MonoFoldable text) => UpdateStateWithSubState LineColumnUnknownBreak (Position dataId text) where
  updateSubStateContext s@(Position {line, column}) ss ss' = if ns == 0
    then s {column = column + cs, subState = ss'}
    else s {line = line + ns, column = cs, subState = ss'}
    where
    (ns, cs) = flip execState (0, 0) $ for_ ss
      $ bool
        (modify $ second (+ 1))
        (modify \(l,_) -> (l + 1, 1))
      . (== fromChar '\n')

instance MonoFoldable text => UpdateStateWithSubState LinesOnly (Position dataId text) where
  updateSubStateContext s@(Position {column}) ss ss' =
    s {column = column + length ss, subState = ss'}

instance MonoFoldable text => UpdateStateWithSubState ColumnsOnly (Position dataId text) where
  updateSubStateContext s@(Position {column}) ss ss' =
    s {column = column + length ss, subState = ss'}

instance ReplaceSubState (Position dataId a) ss (Position dataId ss) where
  replaceSubState p ss = p {subState = ss}

startLineColumn :: text -> Position () text
startLineColumn = Position () 1 1

instance IsString text => IsString (Position () text) where
  fromString = startLineColumn . fromString

instance IsList ss => IsList (Position () ss) where
  type Item (Position () ss) = Item ss
  fromList = startLineColumn . GE.fromList
  toList = GE.toList . subState

-- * Positional Errors

data ErrorPosition dataId = ErrorPosition dataId Int Int String deriving (Show, Eq)

instance E.Error (ErrorPosition dataId) where strMsg = undefined

instance WrapErrorWithState String (Position dataId text) (ErrorPosition dataId) where
  type StateForError String (Position dataId text) (ErrorPosition dataId) = Position dataId text
  wrapErrorWithState' msg (Position dataId l c _) = ErrorPosition dataId l c msg
  stateForError = id

instance ResultMonad (Either (ErrorPosition dataId)) () where
  type ResultingMonad (Either (ErrorPosition dataId)) () = Either (ErrorPosition dataId)
  resultMonad = ()

type EEP dataId e text = Either (ErrorState e (Position dataId text))

instance ChangeMonad () (EEP dataId e text) (Either (ErrorPosition dataId)) where
  type ChangeFunction () (EEP dataId e text) (Either (ErrorPosition dataId)) =
    ErrorState e (Position dataId text) -> (ErrorPosition dataId)
  changeMonad' = first

instance ResultMonad (Either (ErrorState String (Position dataId text))) () where
  type ResultingMonad (Either (ErrorState String (Position dataId text))) () = Either (ErrorPosition dataId)
  resultMonad (ErrorState e s) = wrapErrorWithState e s

-- | "This instance is not sound and is a hack for zoom. The monad conversion in zoom should be more complete or throw away the text entirely but 'catch' in 'MonadError e (StateErrorT s m)' makes this difficult.
data ElementToList
instance ChangeMonad ElementToList (EEP dataId e text) (EEP dataId e [text]) where
  type ChangeFunction ElementToList (EEP dataId e text) (EEP dataId e [text]) = ()
  changeMonad' () = first $ second $ fmap singleton

data ListToElement
instance Monoid text => ChangeMonad ListToElement (EEP dataId e [text]) (EEP dataId e text) where
  type ChangeFunction ListToElement (EEP dataId e [text]) (EEP dataId e text) = ()
  changeMonad' () = first $ second ($> mempty)
  
type instance ErrorContext (LineColumn _) = 'ErrorStateInstance
type instance ErrorContext LinesOnly = 'ErrorStateInstance
type instance ErrorContext ColumnsOnly = 'ErrorStateInstance
type instance ErrorContext LineColumnUnknownBreak = 'ErrorStateInstance

type EESP dataId text = EEP dataId String text
type SE dataId text = StateErrorT 'ErrorStateInstance (Position dataId text) (EESP dataId text)
instance ChangeMonad () EitherString (SE dataId text) where
  type ChangeFunction () EitherString (SE dataId text) = ()
  changeMonad' () = \case
    EValue x -> pure x
    EString msg -> fail msg
    _ -> empty

