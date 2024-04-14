{-# LANGUAGE BangPatterns #-}
module Control.Monad.FileM
  ( File(File)
  , FileM
  , runFileM
  , runFileMWithPath
  ) where

--import Data.ByteString (StrictByteString)
import Control.Monad.Extra (whenM)
import Control.Monad.Reader (ReaderT(ReaderT,runReaderT))
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.Writer.Class (writer, listen)
import Data.ByteString (StrictByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy (LazyByteString)
import Data.ByteString.Lazy qualified as BL
import Data.Text (StrictText)
import Data.Text.IO qualified as TS
import Data.Text.Lazy (LazyText)
import Data.Text.Lazy.IO qualified as TL
import GHC.IO (finally)
import GHC.Num (fromInteger)
import System.IO (IO, Handle, FilePath, IOMode(ReadWriteMode), hClose, openFile, openBinaryFile, hTell, hSeek, SeekMode(AbsoluteSeek), hIsOpen, hSetFileSize)
import System.IO qualified as S
import Data.MonoTraversable (MonoFunctor(omap), MonoFoldable(ofoldMap,ofoldr), MonoTraversable(otraverse))

-- * File

data File a = File deriving (Show, Eq)

type instance Element (File a) = Element a

-- * FileM

type FileM :: Type -> Type -> Type
newtype FileM w a = FileM (ReaderT Handle IO a)
  deriving (Functor, Applicative, Monad, MonadFail, MonadIO)

runFileM :: forall w m a. MonadIO m => Handle -> FileM w a -> m a
runFileM h (FileM x) = liftIO $ runReaderT x h

runFileMWithPath :: forall w m a. (TextOrBinary w, MonadIO m) => FilePath -> FileM w a -> m a
runFileMWithPath fp x = liftIO do
  h <- openFileTOB @w fp ReadWriteMode
  finally (runFileM @w h x) $ whenM (hIsOpen h) $ hClose h

class TextOrBinary w where openFileTOB :: FilePath -> IOMode -> IO Handle
instance TextOrBinary String where openFileTOB = openFile
instance TextOrBinary StrictByteString where openFileTOB = openBinaryFile
instance TextOrBinary LazyByteString where openFileTOB = openBinaryFile

instance MonadWriter StrictByteString (FileM StrictByteString) where
  writer (x, w) = FileM $ ReaderT \h -> do
    BS.hPutStr h w
    return x
  listen m = FileM $ ReaderT \h -> do
    i <- hTell h
    x <- runFileM h m
    i' <- hTell h
    hSeek h AbsoluteSeek i
    w <- BS.hGet h $ fromInteger $ i' - i
    return (x, w)
  pass m = FileM $ ReaderT \h -> do
    i <- hTell h
    (x, f) <- runFileM @StrictByteString h m
    i' <- hTell h
    hSeek h AbsoluteSeek i
    w <- BS.hGet h $ fromInteger $ i' - i
    hSetFileSize h i
    hSeek h AbsoluteSeek i
    BS.hPutStr h $ f w
    return x

instance MonadWriter LazyByteString (FileM LazyByteString) where
  writer (x, w) = FileM $ ReaderT \h -> do
    BL.hPutStr h w
    return x
  listen m = FileM $ ReaderT \h -> do
    i <- hTell h
    x <- runFileM h m
    i' <- hTell h
    hSeek h AbsoluteSeek i
    w <- BL.hGet h $ fromInteger $ i' - i
    return (x, w)
  pass m = FileM $ ReaderT \h -> do
    i <- hTell h
    (x, f) <- runFileM @LazyByteString h m
    i' <- hTell h
    hSeek h AbsoluteSeek i
    w <- BL.hGet h $ fromInteger $ i' - i
    hSetFileSize h i
    hSeek h AbsoluteSeek i
    BL.hPutStr h $ f w
    return x

instance MonadWriter String (FileM String) where
  writer (x, w) = FileM $ ReaderT \h -> do
    S.hPutStr h w
    return x
  --listen (FileM m) = FileM do
  --  h <- ask
  --  i <- liftIO $ hTell h
  --  x <- m
  --  w <- liftIO do
  --    hSeek h AbsoluteSeek i
  --    w <- hGetContents h
  --    length w `seq` return w
  --  return (x, w)
  --pass (FileM m) = FileM do
  --  h <- ask
  --  i <- liftIO $ hTell h
  --  (x, f) <- m
  --  liftIO do
  --    hSeek h AbsoluteSeek i
  --    w <- hGetContents h
  --    hSetFileSize h i
  --    hSeek h AbsoluteSeek i
  --    hPutStr h $ f w
  --  return x

instance MonadWriter StrictText (FileM StrictText) where
  writer (x, w) = FileM $ ReaderT \h -> do
    TS.hPutStr h w
    return x

instance MonadWriter LazyText (FileM LazyText) where
  writer (x, w) = FileM $ ReaderT \h -> do
    TL.hPutStr h w
    return x

instance  OneFw c s (FileM ss) where
  oneFW = FileM $ ReaderT \h -> do
    x <- BS.hGetSome h 1
    case headMay x of
      Just w -> pure w
      Nothing -> fail "Could not take one element. The container is empty."
