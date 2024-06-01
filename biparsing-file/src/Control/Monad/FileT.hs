{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DerivingVia #-}
{-# OPTIONS_GHC -Wno-missing-deriving-strategies #-}
{-# OPTIONS_GHC -Wno-missing-methods #-}
{-# OPTIONS_GHC -Wwarn #-}
module Control.Monad.FileT
  ( FileT(FileT)
  , runFileT
  , runFileTWithPath
  , Open(open)
  , withFile
  ) where

import Data.ByteString.Builder qualified as BB
import Data.ByteString.Lazy qualified as BL
import Data.Text.Lazy.Builder qualified as TB
import Data.Text.Lazy.IO qualified as TL
import System.IO qualified as S
import Control.Monad.RWS.CPS ()
import Control.Monad.Reader.Class (MonadReader(ask))
import Control.Monad.Reader (ReaderT(ReaderT), LiftingReader(LiftingReader))
import Control.Monad.State.Class (MonadState)
import Control.Monad.Trans (MonadTrans)
import Control.Monad.Writer.Class (MonadWriter(writer,listen,pass))
import UnliftIO (finally)

-- * FileT

newtype FileT w m a = FileT' (ReaderT Handle m a)
  deriving newtype (Functor, Applicative, Monad, MonadFail, MonadIO, MonadTrans)
  deriving (MonadReader r) via LiftingReader (ReaderT Handle) m
deriving instance (Alt m, Applicative m) => Alt (FileT w m)

{-# COMPLETE FileT #-}
pattern FileT :: (Handle -> m a) -> FileT w m a
pattern FileT f = FileT' (ReaderT f)

runFileT :: forall w m a. FileT w m a -> Handle -> m a
runFileT (FileT x) = x

runFileTWithPath :: forall w m a. (Open w, MonadUnliftIO m) => FileT w m a -> FilePath -> m a
runFileTWithPath x fp = withFile @w fp ReadWriteMode \h -> runFileT @w x h

-- | May leave hanging file pointers.
withFile :: forall w m a. (Open w, MonadUnliftIO m) => FilePath -> IOMode -> (Handle -> m a) -> m a
withFile fp mode f = do
  h <- liftIO $ open @w fp mode
  finally
    (f h)
    $ liftIO $ whenM (hIsOpen h) $ hClose h

askHandle :: Monad m => FileT w m Handle
askHandle = FileT' ask


--deriving newtype instance MonadTrans (FileT w)
--instance MonadTrans (FileT r w s) where
--  lift = FileT . lift . lift

deriving newtype instance MonadState s m => MonadState s (FileT w m)
--instance Monad m => MonadState s (FileT r w s m) where
--  get = FileT $ lift $ get
--  put = FileT . lift . put

class Open w where open :: FilePath -> IOMode -> IO Handle
instance Open String where open = openFile
instance Open LazyByteString where open = openBinaryFile
instance Open BB.Builder where
  open f m = do
    h <- openBinaryFile f m
    S.hSetBuffering h $ S.BlockBuffering Nothing
    return h
instance Open LazyText where open = openFile
instance Open TB.Builder where open = openFile

instance MonadIO m => MonadWriter String (FileT String m) where
  writer (x, w) = do
    h <- askHandle
    liftIO $ S.hPutStr h w
    return x

instance MonadIO m => MonadWriter BB.Builder (FileT BB.Builder m) where
  writer (x, w) = do
    h <- askHandle
    liftIO $ BB.hPutBuilder h w
    return x
  listen m = do
    h <- askHandle
    i <- liftIO $ hTell h
    x <- m
    w <- liftIO do
      i' <- hTell h
      hSeek h AbsoluteSeek i
      BB.lazyByteString <$< BL.hGet h . fromInteger $ i' - i
    return (x, w)
  pass m = do
    h <- askHandle
    i <- liftIO $ hTell h
    (x, f) <- m
    liftIO do
      i' <- hTell h
      hSeek h AbsoluteSeek i
      w <- BB.lazyByteString <$< BL.hGet h . fromInteger $ i' - i
      hSetFileSize h i
      hSeek h AbsoluteSeek i
      BB.hPutBuilder h $ f w
    return x

instance MonadIO m => MonadWriter TB.Builder (FileT TB.Builder m) where
  writer (x, w) = do
    h <- askHandle
    liftIO $ TL.hPutStr h $ TB.toLazyText w
    return x

(<$<) :: Functor f => (b -> c) -> (a -> f b) -> a -> f c
f <$< g = (f <$>) . g
infixr 4 <$<

--instance MonadIO m => MonadWriter LazyByteString (FileT LazyByteString m) where
--  writer (x, w) = do
--    h <- askHandle
--    liftIO $ BL.hPutStr h w
--    return x
--  listen m = do
--    h <- askHandle
--    i <- liftIO $ hTell h
--    x <- m
--    w <- liftIO do
--      i' <- hTell h
--      hSeek h AbsoluteSeek i
--      BL.hGet h $ fromInteger $ i' - i
--    return (x, w)
--  pass m = do
--    h <- askHandle
--    i <- liftIO $ hTell h
--    (x, f) <- m
--    i' <- liftIO
--      $  hTell h
--      <* hSeek h AbsoluteSeek i
--    liftIO do
--      w <- BL.hGet h $ fromInteger $ i' - i
--      hSetFileSize h i
--      hSeek h AbsoluteSeek i
--      BL.hPutStr h $ f w
--    return x
--
--instance MonadIO m => MonadWriter BB.Builder (FileT BB.Builder m) where
--  writer (x, w) = do
--    h <- askHandle
--    liftIO $ BL.hPutStr h w
--    return x
--  listen m = do
--    h <- askHandle
--    i <- liftIO $ hTell h
--    x <- m
--    w <- liftIO do
--      i' <- hTell h
--      hSeek h AbsoluteSeek i
--      BL.hGet h $ fromInteger $ i' - i
--    return (x, w)
--  pass m = do
--    h <- askHandle
--    i <- liftIO $ hTell h
--    (x, f) <- m
--    i' <- liftIO
--      $  hTell h
--      <* hSeek h AbsoluteSeek i
--    liftIO do
--      w <- BL.hGet h $ fromInteger $ i' - i
--      hSetFileSize h i
--      hSeek h AbsoluteSeek i
--      BL.hPutStr h $ f w
--    return x
--
--instance MonadIO m => MonadWriter StrictText (FileT StrictText m) where
--  writer (x, w) = do
--    h <- askHandle
--    liftIO $ TS.hPutStr h w
--    return x
--
--instance MonadIO m => MonadWriter LazyText (FileT LazyText m) where
--  writer (x, w) = do
--    h <- askHandle
--    liftIO $ TL.hPutStr h w
--    return x

