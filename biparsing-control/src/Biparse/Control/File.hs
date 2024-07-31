{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ConstraintKinds #-}
module Biparse.Control.File (
FileT,
runFileT,
OpenFrom,
OpenFile(..),
IOMode(..),
UpdateState(..),
) where

import System.IO qualified
import System.IO (FilePath, Handle, IOMode(ReadMode,WriteMode), IO, hClose, hGetPosn, hSetPosn)
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

import Control.Monad.Reader (MonadReader, asks, ReaderT(ReaderT,runReaderT), LiftingReader(LiftingReader))
import Control.Monad.Writer.Class (MonadWriter)
import Control.Monad.State.Class (MonadState(get,put), modify)
import Biparse.Core.Update (UpdateStateWithElement(updateStateWithElement))
import Data.Kind (Constraint)

newtype FileT text m a = FileT (ReaderT (FilePath, Handle) m a)
  deriving (Functor, Applicative, Monad, MonadFail, MonadIO)
  deriving (MonadReader r) via LiftingReader (ReaderT (FilePath, Handle)) m
deriving instance MonadWriter w m => MonadWriter w (FileT text m)
deriving instance MonadState s m => MonadState s (FileT text m)
deriving instance MonadError e m => MonadError e (FileT text m)
instance Alt m => Alt (FileT text m) where FileT x <!> FileT y = FileT $ x <!> y

getHandle :: Monad m => FileT text m Handle
getHandle = FileT $ asks snd

runFileT :: forall text m a. (MonadMask m, MonadIO m, OpenFrom text) => FileT text m a -> FilePath -> IOMode -> m a
runFileT (FileT x) fp mode = do
  h <- liftIO $ openFile @(OpenWith text) fp mode
  finally
    (runReaderT x (fp,h))
    (liftIO $ hClose h)

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

getChar :: forall text m char. (MonadFileGetChar char, MonadIO m) => FileT text m char
getChar = liftIO . hGetChar =<< getHandle
class MonadFileGetChar char where hGetChar :: Handle -> IO char
instance MonadFileGetChar Char where hGetChar = System.IO.hGetChar

putChar :: (MonadFilePutChar char, MonadIO m) => char -> FileT text m ()
putChar c = do
  h <- getHandle
  liftIO $ hPutChar h c
class MonadFilePutChar char where hPutChar :: Handle -> char -> IO ()
instance MonadFilePutChar Char where hPutChar = System.IO.hPutChar

newtype UpdateState m a = UpdateState {runUpdateState :: m a}
  deriving (Functor, Applicative, Monad, MonadFail, MonadIO, MonadThrow, MonadCatch, MonadMask, Peek)
deriving instance MonadState s m => MonadState s (UpdateState m)
deriving instance MonadError e m => MonadError e (UpdateState m)
instance Alt m => Alt (UpdateState m) where UpdateState x <!> UpdateState y = UpdateState $ x <!> y

instance (MonadFileGetChar char, Element text ~ char) => OneFwd char (FileT text IO) where
  oneFwd = getChar

instance (MonadFilePutChar char, MonadIO m, Element text ~ char) => OneBwd char (FileT text m) where
  oneBwd = putChar

instance (UpdateStateWithElement s char, MonadFileGetChar char, Element text ~ char, MonadState s m, MonadIO m) => OneFwd char (FileT text (UpdateState m)) where
  oneFwd = do
    c <- getChar
    modify $ updateStateWithElement c
    return c

-- WRONG!!!
--instance (Peek m, MonadIO m) => Peek (FileT text m) where
--  peek (FileT (ReaderT x)) = FileT $ ReaderT \r@(_,h) -> do
--    p <- liftIO $ hGetPosn h
--    y <- peek $ x r
--    liftIO $ hSetPosn p
--    return y

instance (MonadState s m, MonadError e m, MonadState s m, MonadIO m) => Try (FileT text (UpdateState m)) where
  try x = do
    p <- liftIO . hGetPosn =<< getHandle
    s <- get
    catchError x \e -> do
      liftIO $ hSetPosn p
      put s
      throwError e



