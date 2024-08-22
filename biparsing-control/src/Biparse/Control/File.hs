{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE PolyKinds #-}
module Biparse.Control.File (
FileT(FileT, FileT'),
FileT',
runFileT,
Mode,
OpenFrom,
OpenWith,
OpenFile(..),
IOMode(..),
MonadFileGetChar,
--UpdateState(..),
) where

import GHC.TypeLits (TypeError, ErrorMessage(Text))
import System.IO qualified
import System.IO (FilePath, Handle, IOMode(ReadMode,WriteMode,ReadWriteMode,AppendMode), IO, hClose, hGetPosn, hSetPosn, hGetBuf, hPutBuf)
import Data.Text.IO qualified
import Data.Text.Lazy.IO qualified
import Data.Text (StrictText)
import Data.Text.Lazy (LazyText)
import Data.Text.Lazy.Builder qualified
import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified
import Data.ByteString.Builder qualified
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.Catch (MonadThrow, MonadCatch, MonadMask, finally)
import Control.Monad.Identity (IdentityT)
import Control.Monad.Trans.Control (MonadTransControl(StT,liftWith,restoreT), liftThrough)
import Data.Maybe (Maybe(Just,Nothing))

import Control.Monad.RWS (RWST(RWST,runRWST), MonadReader(ask), asks, LiftingReader(LiftingReader), LiftingWriter, LiftWriter(LiftWriter), LiftWriterRWS(LiftWriterRWS), LiftingState(LiftingState))
import Control.Monad.Writer.Class (MonadWriter)
import Control.Monad.State.Class (MonadState(get,put), modify)
import Biparse.Core.Update (UpdateStateWithElement(updateStateWithElement))
import Data.Kind (Constraint)
import Data.Word (Word8)
import Foreign (allocaBytes, sizeOf, poke)
import Foreign qualified
import System.IO.Error (ioError, eofErrorType, mkIOError)

import Biparse.Control.Fwd (Fwd(Fwd))

newtype FileT (d :: Maybe Direction) (mode :: IOMode) text m a = FileT' (FileT' text m a)
  deriving (Functor, Applicative, Alternative, Monad, MonadFail, MonadIO, MonadTrans, MonadThrow, MonadCatch, MonadMask)
  deriving (MonadReader r) via LiftingReader (FileT' text) m
  deriving (MonadWriter w) via LiftingWriter (FileT' text) m
  deriving (MonadState s) via LiftingState (FileT' text) m
type FileT' text = RWST (FilePath, Handle) () [text]
deriving instance MonadError e m => MonadError e (FileT d mode text m)
deriving instance MonadTransControl (FileT d mode text)

runFileT :: forall d mode text m a. (MonadMask m, MonadIO m, OpenFrom text, Mode mode) => FileT d mode text m a -> FilePath -> m a
runFileT (FileT x) fp = do
  h <- liftIO $ openFile @(OpenWith text) fp $ mode @mode
  finally
    (fst <$> x fp h mempty)
    (liftIO $ hClose h)

unFileT :: Functor m => FileT d mode text m a -> FilePath -> Handle -> [text] -> m (a, [text])
unFileT (FileT x) = x

pattern FileT :: Functor m => (FilePath -> Handle -> [text] -> m (a, [text])) -> FileT d mode text m a
pattern FileT x <- FileT' (RWST ((\f fp h s -> f (fp,h) s <&> \(x,y,_) -> (x,y)) -> x)) where
  FileT x = FileT' $ RWST $ (\f (fp,h) s -> f fp h s <&> \(x,y) -> (x,y,())) x

askHandle :: Monad m => FileT d mode text m Handle
askHandle = FileT' $ asks snd

askFile :: Monad m => FileT d mode text m (FilePath, Handle)
askFile = FileT' ask

push :: Monad m => text -> FileT Nothing AppendMode text m ()
push = FileT' . modify . (:)

pop :: MonadFail m => FileT Nothing AppendMode text m text
pop = FileT' $ get >>= \case
  x:xs -> x <$ put xs
  [] -> fail "Pop on empty stack"

class Mode (mode :: IOMode) where mode :: IOMode
instance Mode 'ReadMode where mode = ReadMode
instance Mode 'WriteMode where mode = WriteMode
instance Mode 'ReadWriteMode where mode = ReadWriteMode
instance Mode 'AppendMode where mode = AppendMode

type OpenFrom text = OpenFile (OpenWith text)
data OpenType = Character | Binary
type OpenWith :: Type -> OpenType
type family OpenWith t
type instance OpenWith String = Character
type instance OpenWith StrictText = Character
type instance OpenWith LazyText = Character
type instance OpenWith Data.Text.Lazy.Builder.Builder = Character
type instance OpenWith ByteString = Binary
type instance OpenWith Data.ByteString.Lazy.ByteString = Binary
type instance OpenWith Data.ByteString.Builder.Builder = Binary
type OpenFile :: OpenType -> Constraint
class OpenFile ot where openFile :: FilePath -> IOMode -> IO Handle
instance OpenFile Character where openFile = System.IO.openFile
instance OpenFile Binary where openFile = System.IO.openBinaryFile

getChar :: forall d mode text m char. (MonadFileGetChar char, MonadIO m, ReadRequired mode) => FileT d mode text m char
getChar = liftIO . uncurry hGetChar =<< askFile
{-# WARNING hGetChar, hPutChar "This is hella slow please fix" #-} 
class MonadFileGetChar char where hGetChar :: FilePath -> Handle -> IO char
instance MonadFileGetChar Char where hGetChar = const System.IO.hGetChar
instance MonadFileGetChar Word8 where
  hGetChar fp h = allocaBytes s \p -> do
    c <- hGetBuf h p s 
    if c == s
      then Foreign.peek p
      else liftIO $ ioError $ mkIOError eofErrorType "hGetChar" (pure h) (pure fp)
    where
    s = sizeOf @Word8 0

type ReadRequired :: IOMode -> Constraint
type family ReadRequired mode where
  ReadRequired WriteMode = TypeError (Text "A read mode is required")
  ReadRequired AppendMode = TypeError (Text "A read mode is required")
  ReadRequired _ = ()

putChar :: (MonadFilePutChar char, MonadIO m, WriteRequired mode) => char -> FileT d mode text m ()
putChar c = do
  h <- askHandle
  liftIO $ hPutChar h c
class MonadFilePutChar char where hPutChar :: Handle -> char -> IO ()
instance MonadFilePutChar Char where hPutChar = System.IO.hPutChar
instance MonadFilePutChar Word8 where
  hPutChar h c = allocaBytes s \p -> do
    poke p c
    hPutBuf h p s
    where
    s = sizeOf @Word8 0

putStr :: (MonadFilePutStr text, MonadIO m, WriteRequired mode) => text -> FileT d mode text m ()
putStr str = do
  h <- askHandle
  liftIO $ hPutStr h str
class MonadFilePutStr str where hPutStr :: Handle -> str -> IO ()
instance MonadFilePutStr String where hPutStr = System.IO.hPutStr

type WriteRequired :: IOMode -> Constraint
type family WriteRequired mode where
  WriteRequired ReadMode = TypeError (Text "A write mode is required")
  WriteRequired _ = ()

instance (UpdateStateWithElement s char, MonadFileGetChar char, Element text ~ char, MonadState s m, MonadIO m) => OneFwd char (FileT Nothing ReadMode text m) where
  oneFwd = do
    c <- getChar
    modify $ updateStateWithElement c
    return c

instance (UpdateStateWithElement s char, MonadFileGetChar char, Element text ~ char, MonadState s m, MonadIO m) => OneFwd char (FileT ('Just 'Forward) 'ReadWriteMode text m) where
  oneFwd = do
    c <- getChar
    modify $ updateStateWithElement c
    return c

instance (MonadFilePutChar char, MonadIO m, Element text ~ char) => OneBwd char (FileT 'Nothing 'WriteMode text m) where
  oneBwd = putChar

instance (MonadFilePutChar char, MonadIO m, Element text ~ char) => OneBwd char (FileT ('Just 'Backward) 'ReadWriteMode text m) where
  oneBwd = putChar

instance (MonadFilePutChar char, MonadIO m, Element text ~ char) => OneBwd char (FileT Nothing AppendMode text m) where
  oneBwd = putChar

instance (MonadIO m, MonadMask m, Peek m) => Peek (FileT Nothing ReadMode text m) where
  peek x = do
    p <- liftIO . hGetPosn =<< askHandle
    finally (liftThrough peek x) $ liftIO $ hSetPosn p

instance (MonadIO m, MonadMask m, Peek m) => Peek (FileT ('Just 'Forward) 'ReadWriteMode text m) where
  peek x = do
    p <- liftIO . hGetPosn =<< askHandle
    finally (liftThrough peek x) $ liftIO $ hSetPosn p

instance (MonadIO m, MonadMask m, Try m, OnError m) => Try (FileT Nothing ReadMode text m) where
  try x = do
    p <- liftIO . hGetPosn =<< askHandle
    onError (liftThrough try x) $ liftIO $ hSetPosn p

instance (MonadIO m, MonadMask m, Try m, OnError m) => Try (FileT ('Just 'Forward) 'ReadWriteMode text m) where
  try x = do
    p <- liftIO . hGetPosn =<< askHandle
    onError (liftThrough try x) $ liftIO $ hSetPosn p

instance (MonadFilePutStr text, MonadFail m, OnError m, MonadIO m, Try m, Monoid text) => Try (FileT Nothing AppendMode text m) where
  try x = do
    push mempty
    y <- try x
    z <- pop
    onError
      do
        z' <- pop
        push $ z' <> z
      $ putStr z
    return y

instance OnError m => OnError (FileT d mode text m) where
  onError (FileT x) (FileT y) = FileT \fp h s -> onError (x fp h s) (y fp h s)
