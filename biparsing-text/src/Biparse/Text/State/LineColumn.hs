{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
module Biparse.Text.State.LineColumn (
LinesOnly,
ColumnsOnly,
NoUpdate,
Position(Position),
dataId,
line,
column,
ElementToList,
ListToElement,
) where

import Biparse.State.Lenses (HasDataId(dataId), lens, makeLensesFor)
import Biparse.Core.Update (UpdateStateWithElement(updateStateWithElement), UpdateStateWithSequence(updateStateWithSequence))
import Data.MonoTraversable (MonoFoldable)
import Data.MonoTraversable.Unprefixed (length)
import Lens.Micro ((+~))


-- * Contexts

data LinesOnly
data ColumnsOnly
data NoUpdate

-- * Position state

data Position context dataId = Position
  { _dataId :: dataId -- ^ typically () or FilePath
  , _line :: Int
  , _column :: Int
  } deriving (Show, Eq)
instance HasDataId a (Position context a) (Position context b) b where dataId = lens _dataId \x y -> x {_dataId = y}
$(makeLensesFor [("_line","line"),("_column","column")] ''Position)

instance Default dataId => Default (Position context dataId) where
  def = Position def 1 1

instance (Eq char, IsChar char) => UpdateStateWithElement (Position () dataId) char where
  updateStateWithElement c =
    if c == fromChar '\n'
    then (line %~ succ) . (column .~ 1)
    else column %~ succ

instance UpdateStateWithElement (Position NoUpdate dataId) a where
  updateStateWithElement = const id

instance UpdateStateWithElement (Position LinesOnly dataId) a where
  updateStateWithElement = const $ (line %~ succ) . (column .~ 1)

--instance (Eq (Element text), IsChar (Element text), IsSequence text) => UpdateStateWithElement WindowsLC (Position dataId text) where
--  updateStateWithElement s c ss = case headTailAlt ss of
--    Just (c',ss') | c == fromChar '\r' && c' == fromChar '\n' -> s & line %~ succ & column .~ 1
--    _ -> s & column %~ succ

--instance (Eq (Element text), IsChar (Element text), IsSequence text) => UpdateStateWithElement (Position LineColumnUnknownBreak dataId) text where
--  updateStateWithElement s@(Position {_line = l, _column = c}) ss = if l == l' && c == c' then w else u
--    where
--    u@(Position {_line = l', _column = c'}) = updateStateWithElement @(Position UnixLC dataId) s ss
--    w = updateStateWithElement @(Position WindowsLC dataId) s ss

--instance UpdateStateWithElement (Position LinesOnly dataId) [text] where
--  updateStateWithElement p _ = p & line %~ succ & column .~ 1

instance UpdateStateWithElement (Position ColumnsOnly dataId) text where
  updateStateWithElement = const $ column +~ 1

--instance (Eq (Element text), IsChar (Element text), MonoFoldable text) => UpdateStateWithSequence (LineColumn lb) (Position dataId text) where
--  updateStateWithSequence = updateStateWithSequence @LineColumnUnknownBreak

--instance (Eq (Element text), IsChar (Element text), MonoFoldable text) => UpdateStateWithSequence (Position LineColumnUnknownBreak dataId) text where
--  updateStateWithSequence s ss = bool
--    ((line +~ ns) . (column .~ cs))
--    (column +~ cs)
--    (ns == 0)
--    s
--    where
--    (ns, cs) = flip execState (0, 0) $ for_ ss
--      $ modify
--      . bool
--        (second succ)
--        ((, 1) . succ . fst)
--      . (== fromChar '\n')

instance MonoFoldable text => UpdateStateWithSequence (Position LinesOnly dataId) text where
  updateStateWithSequence ss = line +~ length ss

instance MonoFoldable text => UpdateStateWithSequence (Position ColumnsOnly dataId) text where
  updateStateWithSequence ss = column +~ length ss

-- * Positional Errors

--data ErrorPosition dataId = ErrorPosition dataId Word Word String deriving (Show, Eq)
--
--
--instance ChangeMonad () (EEP dataId e text) (Either (ErrorPosition dataId)) where
--  changeMonad' = first
--type instance ChangeFunction () (EEP dataId e text) (Either (ErrorPosition dataId)) = ErrorState e (Position dataId text) -> (ErrorPosition dataId)
--
--instance ResultMonad (Either (ErrorState String (Position dataId text))) () where
--  type ResultingMonad (Either (ErrorState String (Position dataId text))) () = Either (ErrorPosition dataId)
--  resultMonad (ErrorState e (Position d l c _)) = ErrorPosition d l c e
--
---- | "This instance is not sound and is a hack for zoom. The monad conversion in zoom should be more complete or throw away the text entirely but 'catch' in 'MonadError e (StateErrorT s m)' makes this difficult.
data ElementToList
--instance ChangeMonad ElementToList (EEP dataId e text) (EEP dataId e [text]) where
--  changeMonad' () = first $ second $ fmap singleton
--type instance ChangeFunction ElementToList (EEP dataId e text) (EEP dataId e [text]) = ()
--
--instance ChangeMonad (LineColumn lb) (StateErrorT 'ErrorStateInstance (Position d [text]) (EEP d e [text])) (StateErrorT 'ErrorStateInstance (Position d text) (EEP d e text)) where
--  changeMonad' (f,g) (StateErrorT h) = StateErrorT \s -> h (s & subState %~ f) & _Left . errorState . subState %~ g & _Right . _2 . subState %~ g
--type instance ChangeFunction (LineColumn _) (StateErrorT 'ErrorStateInstance (Position d [text]) (EEP d e [text])) (StateErrorT 'ErrorStateInstance (Position d text) (EEP d e text)) = (text -> [text], [text] -> text)
--
data ListToElement
--instance Monoid text => ChangeMonad ListToElement (EEP dataId e [text]) (EEP dataId e text) where
--  changeMonad' () = first $ second ($> mempty)
--type instance ChangeFunction ListToElement (EEP dataId e [text]) (EEP dataId e text) = ()
--
--type SE dataId text = StateErrorT 'ErrorStateInstance (Position dataId text) (EESP dataId text)
--instance ChangeMonad () EitherString (SE dataId text) where
--  changeMonad' () = \case
--    EValue x -> pure x
--    EString msg -> fail msg
--type instance ChangeFunction () EitherString (SE _ _) = ()
--
--type instance ChangeFunction ListToElement (_ (UndefinedBackwards text)) (_ (UndefinedBackwards text)) = [text] -> text

--type instance ChangeFunction (LineColumn _) (RWST r [text] w m) (RWST r text w m) = [text] -> text

-- * Line Break

--instance
--  ( MonadState (Position d text) m
--  , MonadWriter w n
--  , EqElement text
--  , IsChar (Element text)
--  , ConvertSequence c [text] seq m
--  , IsSequence seq
--  , ConvertElement c (Element text) w n
--  , ConvertSequence c (Element seq) w n
--  , KnownChar char
--  , text ~ SubState (Position d text)
--  ) => LineSplitter ('Left char) 'False c m n (Position d text) seq where
--  lineSplitter = Biparser
--    do
--      p <- get
--      put $ p & subState .~ mempty
--      case splitElem c $ p ^. subState of
--        [x] | null x -> pure mempty
--        x -> convertSequence @c x
--    \ls -> do
--      tell =<< intersperseConvert @c c ls
--      -- tell <=< convertSequence @c $ intersperse (singleton c) ls
--      pure ls
--    where
--    c = char @char

