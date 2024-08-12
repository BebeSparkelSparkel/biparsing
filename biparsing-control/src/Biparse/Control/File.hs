{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ConstraintKinds #-}
module Biparse.Control.File (
FileT,
runFileT,
Mode,
OpenFrom,
OpenWith,
OpenFile(..),
IOMode(..),
MonadFileGetChar,
--UpdateState(..),
) where

import System.IO qualified
import System.IO (FilePath, Handle, IOMode(ReadMode,AppendMode), IO, hClose, hGetPosn, hSetPosn, hGetBuf, hPutBuf)
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

import Control.Monad.Reader (MonadReader(ask), asks, ReaderT(ReaderT,runReaderT), LiftingReader(LiftingReader))
import Control.Monad.Writer.Class (MonadWriter)
import Control.Monad.State.Class (MonadState(get,put), modify)
import Biparse.Core.Update (UpdateStateWithElement(updateStateWithElement))
import Data.Kind (Constraint)
import Data.Tuple (curry)
import Data.Word (Word8)
import Foreign (allocaBytes, sizeOf, poke)
import Foreign qualified
import System.IO.Error (ioError, eofErrorType, mkIOError)

newtype FileT (mode :: IOMode) text m a = FileT' (ReaderT (FilePath, Handle) m a)
  deriving (Functor, Applicative, Monad, MonadFail, MonadIO, MonadTrans, MonadThrow, MonadCatch, MonadMask)
  deriving (MonadReader r) via LiftingReader (ReaderT (FilePath, Handle)) m
deriving instance MonadWriter w m => MonadWriter w (FileT mode text m)
deriving instance MonadState s m => MonadState s (FileT mode text m)
deriving instance MonadError e m => MonadError e (FileT mode text m)
instance Alt m => Alt (FileT mode text m) where FileT' x <!> FileT' y = FileT' $ x <!> y
instance MonadTransControl (FileT mode text) where
  type StT (FileT mode text) a = a
  liftWith f = FileT \fp h -> f \x -> unFileT x fp h
  restoreT = FileT' . ReaderT . const

unFileT :: FileT mode text m a -> FilePath -> Handle -> m a
unFileT (FileT x) = x

pattern FileT :: (FilePath -> Handle -> m a) -> FileT mode text m a
pattern FileT x <- FileT' (ReaderT (curry -> x)) where
  FileT x = FileT' $ ReaderT $ uncurry x

getHandle :: Monad m => FileT mode text m Handle
getHandle = FileT' $ asks snd

askFile :: Monad m => FileT mode text m (FilePath, Handle)
askFile = FileT' ask

runFileT :: forall mode text m a. (MonadMask m, MonadIO m, OpenFrom text, Mode mode) => FileT mode text m a -> FilePath -> m a
runFileT (FileT x) fp = do
  h <- liftIO $ openFile @(OpenWith text) fp $ mode @mode
  finally
    (x fp h)
    (liftIO $ hClose h)

class Mode (mode :: IOMode) where mode :: IOMode
instance Mode 'ReadMode where mode = ReadMode
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

getChar :: forall mode text m char. (MonadFileGetChar char, MonadIO m) => FileT mode text m char
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

putChar :: (MonadFilePutChar char, MonadIO m) => char -> FileT mode text m ()
putChar c = do
  h <- getHandle
  liftIO $ hPutChar h c
class MonadFilePutChar char where hPutChar :: Handle -> char -> IO ()
instance MonadFilePutChar Char where hPutChar = System.IO.hPutChar
instance MonadFilePutChar Word8 where
  hPutChar h c = allocaBytes s \p -> do
    poke p c
    hPutBuf h p s
    where
    s = sizeOf @Word8 0

instance (UpdateStateWithElement s char, MonadFileGetChar char, Element text ~ char, MonadState s m, MonadIO m) => OneFwd char (FileT ReadMode text m) where
  oneFwd = do
    c <- getChar
    modify $ updateStateWithElement c
    return c

instance (MonadFilePutChar char, MonadIO m, Element text ~ char) => OneBwd char (FileT AppendMode text m) where
  oneBwd = putChar

instance (MonadIO m, MonadMask m, Peek m) => Peek (FileT ReadMode text m) where
  peek x = do
    p <- liftIO . hGetPosn =<< getHandle
    finally (liftThrough peek x) $ liftIO $ hSetPosn p

instance (MonadIO m, MonadMask m, Try m, OnError m) => Try (FileT ReadMode text m) where
  try x = do
    p <- liftIO . hGetPosn =<< getHandle
    onError (liftThrough try x) $ liftIO $ hSetPosn p

instance OnError m => OnError (FileT mode text m) where
  onError (FileT x) (FileT y) = FileT \fp h -> onError (x fp h) (y fp h)
