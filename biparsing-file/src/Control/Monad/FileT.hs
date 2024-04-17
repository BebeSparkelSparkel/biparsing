{-# LANGUAGE AllowAmbiguousTypes #-}
module Control.Monad.FileT
  ( FileT(..)
  , runFileT
  , runFileTWithPath
  , withFile
  , Open
  ) where

import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.Text.IO qualified as TS
import Data.Text.Lazy.IO qualified as TL
import System.IO qualified as S
import Control.Monad.RWS.CPS (RWST, runRWST, MonadState, MonadReader(local,reader), asks, MonadWriter(writer,listen,pass))
import Control.Monad.Reader
import Control.Monad.State.Class
import Control.Monad.Trans

-- * FileT

newtype FileT r w s m a = FileT (ReaderT Handle (RWST r () s m) a)
  deriving newtype (Functor, Applicative, Monad, MonadFail, MonadIO)

runFileT :: forall w r s m a. Functor m => FileT r w s m a -> Handle -> r -> s -> m (a, s)
runFileT (FileT x) h r s = (\(a,s',_) -> (a,s')) <$> runRWST (runReaderT x h) r s

runFileTWithPath :: forall w r s m a. (Open w, MonadUnliftIO m) => FileT r w s m a -> FilePath -> r -> s -> m (a, s)
runFileTWithPath x fp r s = withFile @w fp ReadWriteMode \h -> runFileT @w x h r s

withFile :: forall w m a. (Open w, MonadUnliftIO m) => FilePath -> IOMode -> (Handle -> m a) -> m a
withFile fp mode f = do
  h <- liftIO $ open @w fp mode
  finally
    (f h)
    $ liftIO $ whenM (hIsOpen h) $ hClose h

askHandle :: Monad m => FileT r w s m Handle
askHandle = FileT ask

instance MonadTrans (FileT r w s) where
  lift = FileT . lift . lift

instance Monad m => MonadState s (FileT r w s m) where
  get = FileT $ lift $ get
  put = FileT . lift . put

instance Monad m => MonadReader r (FileT r w s m) where
  local f (FileT (ReaderT g)) = FileT $ ReaderT $ local f . g
  reader = FileT . lift . reader

class Open w where open :: FilePath -> IOMode -> IO Handle
instance Open String where open = openFile
instance Open StrictByteString where open = openBinaryFile
instance Open LazyByteString where open = openBinaryFile
instance Open StrictText where open = openFile
instance Open LazyText where open = openFile

--instance MonadIO m => MonadWriter StrictByteString (FileT r StrictByteString s m) where
--  writer (x, w) = do
--    h <- askHandle
--    liftIO $ BS.hPutStr h w
--    return x
--  listen m = do
--    h <- askHandle
--    i <- liftIO $ hTell h
--    x <- m
--    w <- liftIO do
--      i' <- hTell h
--      hSeek h AbsoluteSeek i
--      BS.hGet h $ fromInteger $ i' - i
--    return (x, w)
--  pass m = do
--    h <- askHandle
--    i <- liftIO $ hTell h
--    (x, f) <- m
--    liftIO do
--      i' <- hTell h
--      hSeek h AbsoluteSeek i
--      w <- BS.hGet h $ fromInteger $ i' - i
--      hSetFileSize h i
--      hSeek h AbsoluteSeek i
--      BS.hPutStr h $ f w
--    return x

instance MonadIO m => MonadWriter LazyByteString (FileT r LazyByteString s m) where
  writer (x, w) = do
    h <- askHandle
    liftIO $ BL.hPutStr h w
    return x
  listen m = do
    h <- askHandle
    i <- liftIO $ hTell h
    x <- m
    w <- liftIO do
      i' <- hTell h
      hSeek h AbsoluteSeek i
      BL.hGet h $ fromInteger $ i' - i
    return (x, w)
  pass m = do
    h <- askHandle
    i <- liftIO $ hTell h
    (x, f) <- m
    i' <- liftIO
      $  hTell h
      <* hSeek h AbsoluteSeek i
    liftIO do
      w <- BL.hGet h $ fromInteger $ i' - i
      hSetFileSize h i
      hSeek h AbsoluteSeek i
      BL.hPutStr h $ f w
    return x

--instance MonadIO m => MonadWriter String (FileT r String s m) where
--  writer (x, w) = do
--    h <- askHandle
--    liftIO $ S.hPutStr h w
--    return x
--  --listen (FileT r m s m) = FileT do
--  --  h <- ask
--  --  i <- liftIO $ hTell h
--  --  x <- m
--  --  w <- liftIO do
--  --    hSeek h AbsoluteSeek i
--  --    w <- hGetContents h
--  --    length w `seq` return w
--  --  return (x, w)
--  --pass (FileT r m s m) = FileT do
--  --  h <- ask
--  --  i <- liftIO $ hTell h
--  --  (x, f) <- m
--  --  liftIO do
--  --    hSeek h AbsoluteSeek i
--  --    w <- hGetContents h
--  --    hSetFileSize h i
--  --    hSeek h AbsoluteSeek i
--  --    hPutStr h $ f w
--  --  return x
--
--instance MonadIO m => MonadWriter StrictText (FileT r StrictText s m) where
--  writer (x, w) = do
--    h <- askHandle
--    liftIO $ TS.hPutStr h w
--    return x
--
--instance MonadIO m => MonadWriter LazyText (FileT r LazyText s m) where
--  writer (x, w) = do
--    h <- askHandle
--    liftIO $ TL.hPutStr h w
--    return x

