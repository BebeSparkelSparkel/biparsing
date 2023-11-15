{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Biparse.Text.Context.LineColumn
  ( LineColumn
  , UnixLC
  , WindowsLC
  , LinesOnly
  , ColumnsOnly
  , LineColumnUnknownBreak
  , Position(..)
  , dataId
  , line
  , column
  , subState
  , startLineColumn
  , ErrorPosition(..)
  , EEP
  , EESP
  , ElementToList
  , ListToElement
  ) where

import Biparse.Text.LineBreak (LineBreakType(Unix,Windows))
import Biparse.Biparser (SubState, GetSubState(getSubState), UpdateStateWithElement(updateElementContext), UpdateStateWithSubState(updateSubStateContext), ReplaceSubState(replaceSubState))
import Control.Monad.StateError (StateErrorT(StateErrorT), ErrorState(ErrorState), ErrorContext, ErrorInstance(ErrorStateInstance), WrapErrorWithState(StateForError,wrapErrorWithState',stateForError), wrapErrorWithState, errorState)
import Control.Monad.EitherString (EitherString(EValue,EString))
import GHC.Exts (IsList(Item))
import GHC.Exts qualified as GE
import Control.Monad.ChangeMonad (ChangeMonad, ChangeFunction, changeMonad', ResultMonad(ResultingMonad,resultMonad))
import Control.Lens (makeLenses, (.~), (%~), _2, _Left, _Right)

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
  { _dataId :: dataId -- | could be FilePath
  , _line :: Int
  , _column :: Int
  , _subState :: text
  } deriving (Show, Eq, Functor)
$(makeLenses ''Position)

type instance SubState (LineColumn _)         (Position _ text) = text
type instance SubState LinesOnly              (Position _ text) = text
type instance SubState ColumnsOnly            (Position _ text) = text
type instance SubState LineColumnUnknownBreak (Position _ text) = text

instance GetSubState (LineColumn lb)        (Position dataId text) where getSubState = _subState
instance GetSubState LinesOnly              (Position dataId text) where getSubState = _subState
instance GetSubState ColumnsOnly            (Position dataId text) where getSubState = _subState
instance GetSubState LineColumnUnknownBreak (Position dataId text) where getSubState = _subState

type CharCs text char =
  ( Eq char
  , IsChar char
  , char ~ Element text
  )

instance CharCs text char => UpdateStateWithElement (LineColumn 'Unix) (Position dataId text) where
  updateElementContext s c ss =
    if c == fromChar '\n'
    then s & line %~ (+ 1) & column .~ 1 & subState .~ ss
    else s & column %~ (+ 1) & subState .~ ss

instance (CharCs text char, IsSequence text) => UpdateStateWithElement (LineColumn 'Windows) (Position dataId text) where
  updateElementContext s c ss = case headTailAlt ss of
    Just (c',ss') | c == fromChar '\r' && c' == fromChar '\n' -> s & line %~ (+ 1) & column .~ 1 & subState .~ ss'
    _ -> s & column %~ (+ 1) & subState .~ ss

instance (CharCs text char, IsSequence text) => UpdateStateWithElement LineColumnUnknownBreak (Position dataId text) where
  updateElementContext s@(Position {_line = l, _column = c}) ss ss' = if l == l' && c == c' then w else u
    where
    u@(Position {_line = l', _column = c'}) = updateElementContext @(LineColumn 'Unix) s ss ss'
    w = updateElementContext @(LineColumn 'Windows) s ss ss'

instance UpdateStateWithElement LinesOnly (Position dataId [text]) where
  updateElementContext p _ ss = p & line %~ (+ 1) & column .~ 1 & subState .~ ss

instance UpdateStateWithElement ColumnsOnly (Position dataId text) where
  updateElementContext p _ ss = p & column %~ (+ 1) & subState .~ ss

instance (CharCs text char, MonoFoldable text) => UpdateStateWithSubState (LineColumn lb) (Position dataId text) where
  updateSubStateContext = updateSubStateContext @LineColumnUnknownBreak

instance (CharCs text char, MonoFoldable text) => UpdateStateWithSubState LineColumnUnknownBreak (Position dataId text) where
  updateSubStateContext s ss ss' = if ns == 0
    then s & column %~ (+ cs)
           & subState .~ ss'
    else s & line %~ (+ ns)
           & column .~ cs
           & subState .~ ss'
    where
    (ns, cs) = flip execState (0, 0) $ for_ ss
      $ bool
        (modify $ second (+ 1))
        (modify \(l,_) -> (l + 1, 1))
      . (== fromChar '\n')

instance MonoFoldable text => UpdateStateWithSubState LinesOnly (Position dataId text) where
  updateSubStateContext = updateSubStateContext @ColumnsOnly

instance MonoFoldable text => UpdateStateWithSubState ColumnsOnly (Position dataId text) where
  updateSubStateContext s ss ss' =
    s & column %~ (+ length ss)
      & subState .~ ss'

instance ReplaceSubState (Position dataId a) ss (Position dataId ss) where
  replaceSubState p ss = p & subState .~ ss

startLineColumn :: text -> Position () text
startLineColumn = Position () 1 1

instance IsString text => IsString (Position () text) where
  fromString = startLineColumn . fromString

instance IsList ss => IsList (Position () ss) where
  type Item (Position () ss) = Item ss
  fromList = startLineColumn . GE.fromList
  toList = GE.toList . _subState

-- * Positional Errors

data ErrorPosition dataId = ErrorPosition dataId Int Int String deriving (Show, Eq)

instance E.Error (ErrorPosition dataId) where strMsg = undefined

instance WrapErrorWithState String (Position dataId text) (ErrorPosition dataId) where
  type StateForError String (Position dataId text) (ErrorPosition dataId) = Position dataId text
  wrapErrorWithState' msg (Position d l c _) = ErrorPosition d l c msg
  stateForError = id

instance ResultMonad (Either (ErrorPosition dataId)) () where
  type ResultingMonad (Either (ErrorPosition dataId)) () = Either (ErrorPosition dataId)
  resultMonad = ()

type EEP dataId e text = Either (ErrorState e (Position dataId text))

instance ChangeMonad () (EEP dataId e text) (Either (ErrorPosition dataId)) where
  changeMonad' = first
type instance ChangeFunction () (EEP dataId e text) (Either (ErrorPosition dataId)) = ErrorState e (Position dataId text) -> (ErrorPosition dataId)

instance ResultMonad (Either (ErrorState String (Position dataId text))) () where
  type ResultingMonad (Either (ErrorState String (Position dataId text))) () = Either (ErrorPosition dataId)
  resultMonad (ErrorState e s) = wrapErrorWithState e s

-- | "This instance is not sound and is a hack for zoom. The monad conversion in zoom should be more complete or throw away the text entirely but 'catch' in 'MonadError e (StateErrorT s m)' makes this difficult.
data ElementToList
instance ChangeMonad ElementToList (EEP dataId e text) (EEP dataId e [text]) where
  changeMonad' () = first $ second $ fmap singleton
type instance ChangeFunction ElementToList (EEP dataId e text) (EEP dataId e [text]) = ()

instance ChangeMonad (LineColumn lb) (StateErrorT 'ErrorStateInstance (Position d [text]) (EEP d e [text])) (StateErrorT 'ErrorStateInstance (Position d text) (EEP d e text)) where
  changeMonad' (f,g) (StateErrorT h) = StateErrorT \s -> h (s & subState %~ f) & _Left . errorState . subState %~ g & _Right . _2 . subState %~ g
type instance ChangeFunction (LineColumn _) (StateErrorT 'ErrorStateInstance (Position d [text]) (EEP d e [text])) (StateErrorT 'ErrorStateInstance (Position d text) (EEP d e text)) = (text -> [text], [text] -> text)

data ListToElement
instance Monoid text => ChangeMonad ListToElement (EEP dataId e [text]) (EEP dataId e text) where
  changeMonad' () = first $ second ($> mempty)
type instance ChangeFunction ListToElement (EEP dataId e [text]) (EEP dataId e text) = ()
  
type instance ErrorContext (LineColumn _) = 'ErrorStateInstance
type instance ErrorContext LinesOnly = 'ErrorStateInstance
type instance ErrorContext ColumnsOnly = 'ErrorStateInstance
type instance ErrorContext LineColumnUnknownBreak = 'ErrorStateInstance

type EESP dataId text = EEP dataId String text
type SE dataId text = StateErrorT 'ErrorStateInstance (Position dataId text) (EESP dataId text)
instance ChangeMonad () EitherString (SE dataId text) where
  changeMonad' () = \case
    EValue x -> pure x
    EString msg -> fail msg
type instance ChangeFunction () EitherString (SE _ _) = ()

