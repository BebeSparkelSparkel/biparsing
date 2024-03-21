{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Biparse.Text.Context.LineColumn
  ( LineColumn
  , UnixLC
  , WindowsLC
  , LinesOnly
  , ColumnsOnly
  , LineColumnUnknownBreak
  , NoUpdate
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

import Biparse.Biparser (SubState, GetSubState(getSubState), UpdateStateWithElement(updateElementContext), UpdateStateWithSubState(updateSubStateContext), ReplaceSubState(replaceSubState))
import Biparse.Biparser.Internal (Biparser(Biparser), InitSuperState(SuperState, fromSubState), SuperArg)
import Biparse.Text.LineBreak (LineBreakType(Unix,Windows), LineSplitter, lineSplitter, UpdateSuperState)
import Biparse.Utils (char)
import Control.Lens (makeLenses, (.~), (%~), _2, _Left, _Right, (^.))
import Control.Monad.ChangeMonad (ChangeMonad, ChangeFunction, changeMonad', ResultMonad(ResultingMonad,resultMonad))
import Control.Monad.EitherString (EitherString(EValue,EString))
import Control.Monad.StateError (StateErrorT(StateErrorT), ErrorState(ErrorState), ErrorContext, ErrorInstance(ErrorStateInstance), WrapErrorWithState(StateForError,wrapErrorWithState',stateForError), wrapErrorWithState, errorState)
import Control.Monad.UndefinedBackwards (UndefinedBackwards)
import Data.EqElement (splitElem, splitSeq)
import Data.MonoTraversable.Unprefixed (foldr)
import Data.Sequences (uncons)
import GHC.Exts (IsList(Item))
import GHC.Exts qualified as GE
import System.IO (FilePath)

-- * Contexts
type UnixLC = LineColumn 'Unix
type WindowsLC = LineColumn 'Windows

data LineColumn (lineBreak :: LineBreakType)
data LinesOnly
data ColumnsOnly
data LineColumnUnknownBreak
data NoUpdate

-- * Position state

data Position dataId text = Position
  { _dataId :: dataId -- | could be FilePath
  , _line :: Int
  , _column :: Int
  , _subState :: text
  } deriving (Show, Eq, Functor)
$(makeLenses ''Position)


instance GetSubState (Position dataId text) where
  type SubState (Position _ text) = text
  getSubState = _subState

instance InitSuperState (LineColumn lb) text where
  type SuperState (LineColumn lb) text = Position FilePath text
  fromSubState = fromSubState'
instance InitSuperState LinesOnly text where
  type SuperState LinesOnly text = Position FilePath text
  fromSubState = fromSubState'
instance InitSuperState ColumnsOnly text where
  type SuperState ColumnsOnly text = Position FilePath text
  fromSubState = fromSubState'
instance InitSuperState LineColumnUnknownBreak text where
  type SuperState LineColumnUnknownBreak text = Position FilePath text
  fromSubState = fromSubState'
instance InitSuperState NoUpdate text where
  type SuperState NoUpdate text = Position FilePath text
  fromSubState = fromSubState'
fromSubState' :: d -> text -> Position d text
fromSubState' fp text = startLineColumn @() text & dataId .~ fp
type instance SuperArg (Position d _) = d

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
  updateSubStateContext s ss ss' = s
    & line %~ (+ length ss)
    & subState .~ ss'

instance MonoFoldable text => UpdateStateWithSubState ColumnsOnly (Position dataId text) where
  updateSubStateContext s ss ss' = s
    & column %~ (+ length ss)
    & subState .~ ss'

instance ReplaceSubState (Position dataId a) ss (Position dataId ss) where
  replaceSubState p ss = p & subState .~ ss

startLineColumn :: forall d text. Default d => text -> Position d text
startLineColumn = Position def 1 1

instance (Default d, IsString text) => IsString (Position d text) where
  fromString = startLineColumn . fromString

instance IsList ss => IsList (Position () ss) where
  type Item (Position _ ss) = Item ss
  fromList = startLineColumn . GE.fromList
  toList = GE.toList . _subState

-- * Positional Errors

data ErrorPosition dataId = ErrorPosition dataId Int Int String deriving (Show, Eq)

instance WrapErrorWithState String (Position dataId text) (ErrorPosition dataId) where
  type StateForError String (Position dataId text) (ErrorPosition dataId) = Position dataId text
  wrapErrorWithState' msg (Position d l c _) = ErrorPosition d l c msg
  stateForError = id

--instance ResultMonad (Either (ErrorPosition dataId)) () where
--  type ResultingMonad (Either (ErrorPosition dataId)) () = Either (ErrorPosition dataId)
--  resultMonad = ()

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

type instance ChangeFunction ListToElement (_ (UndefinedBackwards text)) (_ (UndefinedBackwards text)) = [text] -> text

--type instance ChangeFunction (LineColumn _) (RWST r [text] w m) (RWST r text w m) = [text] -> text

-- * Line Break

type instance UpdateSuperState (LineColumn _)         = 'True
type instance UpdateSuperState LinesOnly              = 'True
type instance UpdateSuperState ColumnsOnly            = 'True
type instance UpdateSuperState LineColumnUnknownBreak = 'True
type instance UpdateSuperState NoUpdate               = 'False

instance
  ( MonadState (Position d text) m
  , MonadWriter w n
  , EqElement text
  , IsChar (Element text)
  , ConvertSequence c [text] seq m
  , IsSequence seq
  , ConvertElement c (Element text) w n
  , ConvertSequence c (Element seq) w n
  , KnownChar char
  , text ~ SubState (Position d text)
  ) => LineSplitter ('Left char) 'False c m n (Position d text) seq where
  lineSplitter = Biparser
    do
      p <- get
      put $ p & subState .~ mempty
      case splitElem c $ p ^. subState of
        [x] | null x -> pure mempty
        x -> convertSequence @c x
    \ls -> do
      tell =<< intersperseConvert @c c ls
      -- tell <=< convertSequence @c $ intersperse (singleton c) ls
      pure ls
    where
    c = char @char

intersperseConvert :: forall c e n seq seq'.
  ( IsSequence seq
  , ConvertElement c e seq' n
  , ConvertSequence c (Element seq) seq' n
  , Monoid seq'
  , Monad n
  ) => e -> seq -> n seq'
intersperseConvert x ys = do
  y <- convertElement @c x
  intercalateConvert @c y ys

instance
  ( MonadState (Position d text) m
  , EqElement text
  , MonadWriter w n
  , IsString text
  , IsString w
  , IsSequence seq
  , ConvertSequence c [text] seq m
  , ConvertSequence c (Element seq) w n
  , KnownSymbol sym
  , text ~ SubState (Position d text)
  ) => LineSplitter ('Right sym) 'False c m n (Position d text) seq where
  lineSplitter = Biparser
    do
      p :: Position d text <- get
      put $ p & subState .~ mempty
      case splitSeq sym $ p ^. subState of
        [x] | null x -> pure mempty
        x -> convertSequence @c x
    \ls -> do
      tell =<< intercalateConvert @c sym ls
      pure ls
    where
    sym :: IsString a => a
    sym = symbol @sym

intercalateConvert :: forall c n seq seq'.
  ( IsSequence seq
  , ConvertSequence c (Element seq) seq' n
  , Monoid seq'
  , Applicative n
  ) => seq' -> seq -> n seq'
intercalateConvert inter = uncons >>> maybe (pure mempty) \(h,ts) -> liftA2 (<>)
  (convertSequence @c h)
  $ foldr
      (\x xs -> (inter <>) <$> liftA2 (<>) (convertSequence @c x) xs)
      (pure mempty)
      ts

-- * Convert Instance Contexts

instance Applicative m => ConvertSequence UnixLC                 a a m where convertSequence = pure
instance Applicative m => ConvertSequence WindowsLC              a a m where convertSequence = pure
instance Applicative m => ConvertSequence LinesOnly              a a m where convertSequence = pure
instance Applicative m => ConvertSequence ColumnsOnly            a a m where convertSequence = pure
instance Applicative m => ConvertSequence LineColumnUnknownBreak a a m where convertSequence = pure
instance Applicative m => ConvertSequence NoUpdate               a a m where convertSequence = pure

instance (e ~ Element seq, MonoPointed seq, Applicative m) => ConvertElement UnixLC                 e seq m where convertElement = pure . singleton
instance (e ~ Element seq, MonoPointed seq, Applicative m) => ConvertElement WindowsLC              e seq m where convertElement = pure . singleton
instance (e ~ Element seq, MonoPointed seq, Applicative m) => ConvertElement LinesOnly              e seq m where convertElement = pure . singleton
instance (e ~ Element seq, MonoPointed seq, Applicative m) => ConvertElement ColumnsOnly            e seq m where convertElement = pure . singleton
instance (e ~ Element seq, MonoPointed seq, Applicative m) => ConvertElement LineColumnUnknownBreak e seq m where convertElement = pure . singleton
instance (e ~ Element seq, MonoPointed seq, Applicative m) => ConvertElement NoUpdate               e seq m where convertElement = pure . singleton

