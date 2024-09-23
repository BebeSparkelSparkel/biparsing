{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE PolyKinds #-}
module Biparse.Control.File
(
FileT(FileT, FileT'),
FileT',
runFileT,
Mode,
OpenFrom,
OpenWith,
OpenFile(..),
IOMode(..),
MonadFileGetChar(..),
ReadRequired,
MonadFilePutChar(..),
MonadFilePutStr(..),
WriteRequired,
) where

import Biparse.Control.BP (BP(BP))
import Biparse.Control.Bwd (Bwd(Bwd,runBwd))
import Biparse.Control.Fwd (Fwd(Fwd,runFwd))
import Control.Monad.Catch (MonadThrow, MonadCatch, MonadMask, finally)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.RWS (RWST(RWST), MonadReader(ask), asks, LiftingReader(LiftingReader), LiftingWriter, LiftWriter(LiftWriter), LiftingState(LiftingState))
import Data.ByteString (ByteString)
import Data.ByteString qualified
import Data.ByteString.Builder qualified
import Data.ByteString.Lazy qualified
import Data.Text (StrictText)
import Data.Text.IO qualified
import Data.Text.Lazy (LazyText)
import Data.Text.Lazy.Builder qualified
import Data.Text.Lazy.IO qualified
import Data.Word (Word8)
import Foreign (allocaBytes, sizeOf, poke)
import Foreign qualified
import GHC.TypeLits (TypeError, ErrorMessage(Text))
import System.IO (FilePath, Handle, IOMode(ReadMode,WriteMode,ReadWriteMode,AppendMode), hClose, hGetPosn, hSetPosn, hGetBuf, hPutBuf)
import System.IO qualified
import System.IO.Error (ioError, eofErrorType, mkIOError)

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

{-# COMPLETE FileT #-}
pattern FileT :: Functor m => (FilePath -> Handle -> [text] -> m (a, [text])) -> FileT d mode text m a
pattern FileT x <- FileT' (RWST ((\f fp h s -> f (fp,h) s <&> \(x,y,_) -> (x,y)) -> x)) where
  FileT x = FileT' $ RWST $ (\f (fp,h) s -> f fp h s <&> \(x,y) -> (x,y,())) x

askHandle :: Monad m => FileT d mode text m Handle
askHandle = FileT' $ asks snd

askFile :: Monad m => FileT d mode text m (FilePath, Handle)
askFile = FileT' ask

push :: Monad m => text -> FileT 'Nothing 'AppendMode text m ()
push = FileT' . modify . (:)

pop :: MonadFail m => FileT 'Nothing 'AppendMode text m text
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
type instance OpenWith (_ x) = OpenWith x
type instance OpenWith Char = 'Character
type instance OpenWith Word8 = 'Binary
type instance OpenWith StrictText = 'Character
type instance OpenWith LazyText = 'Character
type instance OpenWith Data.Text.Lazy.Builder.Builder = 'Character
type instance OpenWith ByteString = 'Binary
type instance OpenWith Data.ByteString.Lazy.ByteString = 'Binary
type instance OpenWith Data.ByteString.Builder.Builder = 'Binary
type OpenFile :: OpenType -> Constraint
class OpenFile ot where openFile :: FilePath -> IOMode -> IO Handle
instance OpenFile 'Character where openFile = System.IO.openFile
instance OpenFile 'Binary where openFile = System.IO.openBinaryFile

getChar :: forall char d mode text m. (MonadFileGetChar mode char, MonadIO m) => FileT d mode text m char
getChar = liftIO . uncurry (hGetChar @mode) =<< askFile
{-# WARNING hGetChar, hPutChar "This is hella slow please fix" #-} 
class ReadRequired mode => MonadFileGetChar mode char where hGetChar :: FilePath -> Handle -> IO char
instance ReadRequired mode => MonadFileGetChar mode Char where hGetChar = const System.IO.hGetChar
instance ReadRequired mode => MonadFileGetChar mode Word8 where
  hGetChar fp h = allocaBytes s \p -> do
    c <- hGetBuf h p s 
    if c == s
      then Foreign.peek p
      else liftIO $ ioError $ mkIOError eofErrorType "hGetChar" (pure h) (pure fp)
    where
    s = sizeOf @Word8 0

type ReadRequired :: IOMode -> Constraint
class ReadRequired mode where
instance ReadRequired 'ReadMode
instance ReadRequired 'ReadWriteMode
instance TypeError ('Text "A read mode is required") => ReadRequired 'WriteMode
instance TypeError ('Text "A read mode is required") => ReadRequired 'AppendMode

putChar :: forall mode char m d text. (MonadFilePutChar mode char, MonadIO m) => char -> FileT d mode text m ()
putChar c = do
  h <- askHandle
  liftIO $ hPutChar @mode h c
class WriteRequired mode => MonadFilePutChar mode char where hPutChar :: Handle -> char -> IO ()
instance WriteRequired mode => MonadFilePutChar mode Char where hPutChar = System.IO.hPutChar
instance WriteRequired mode => MonadFilePutChar mode Word8 where
  hPutChar h c = allocaBytes s \p -> do
    poke p c
    hPutBuf h p s
    where
    s = sizeOf @Word8 0

putStr :: forall mode m d text. (MonadFilePutStr mode text, MonadIO m) => text -> FileT d mode text m ()
putStr str = do
  h <- askHandle
  liftIO $ hPutStr @mode h str
class WriteRequired mode => MonadFilePutStr mode str where hPutStr :: Handle -> str -> IO ()
instance WriteRequired mode => MonadFilePutStr mode String where hPutStr = System.IO.hPutStr
instance WriteRequired mode => MonadFilePutStr mode ByteString where hPutStr = Data.ByteString.hPutStr
instance WriteRequired mode => MonadFilePutStr mode [Word8] where hPutStr h = Data.ByteString.hPutStr h . Data.ByteString.pack
instance WriteRequired mode => MonadFilePutStr mode Data.ByteString.Lazy.ByteString where hPutStr = Data.ByteString.Lazy.hPutStr
instance WriteRequired mode => MonadFilePutStr mode Data.ByteString.Builder.Builder where hPutStr h = Data.ByteString.Lazy.hPutStr h . Data.ByteString.Builder.toLazyByteString
instance WriteRequired mode => MonadFilePutStr mode StrictText where hPutStr = Data.Text.IO.hPutStr
instance WriteRequired mode => MonadFilePutStr mode LazyText where hPutStr = Data.Text.Lazy.IO.hPutStr
instance WriteRequired mode => MonadFilePutStr mode Data.Text.Lazy.Builder.Builder where hPutStr h = Data.Text.Lazy.IO.hPutStr h . Data.Text.Lazy.Builder.toLazyText

type WriteRequired :: IOMode -> Constraint
class WriteRequired mode where
instance TypeError ('Text "A write mode is required") => WriteRequired 'ReadMode
instance WriteRequired 'WriteMode
instance WriteRequired 'ReadWriteMode
instance WriteRequired 'AppendMode

instance (One (Fwd m char char) (Fwd ((->) (Fwd m char char)) char), UpdateStateWithElement s char, MonadFileGetChar 'ReadMode char, Element text ~ char, MonadState s m, MonadIO m) => One char (Fwd (BP (FileT 'Nothing 'ReadMode text m)) char) where one = oneGetChar
instance (One (Fwd m char char) (Fwd ((->) (Fwd m char char)) char), UpdateStateWithElement s char, MonadFileGetChar 'ReadWriteMode char, Element text ~ char, MonadState s m, MonadIO m) => One char (Fwd (BP (FileT ('Just 'Forward) 'ReadWriteMode text m)) char) where one = oneGetChar
oneGetChar :: forall d mode text m char s.
  ( MonadState s m
  , MonadIO m
  , UpdateStateWithElement s char
  , One (Fwd m char char) (Fwd ((->) (Fwd m char char)) char)
  , MonadFileGetChar mode char
  ) => Fwd (BP (FileT d mode text m)) char char
oneGetChar = Fwd $ BP do
  c <- getChar @char
  modify $ updateStateWithElement c
  lift $ runFwd @_ @char $ runFwd @_ @char one $ pure @(Fwd m char) c

instance (MonadFilePutChar 'WriteMode char, MonadIO m, Element text ~ char, One char (Bwd m char)) => One char (Bwd (BP (FileT 'Nothing 'WriteMode text m)) char) where one = onePutChar
instance (MonadFilePutChar 'ReadWriteMode char, MonadIO m, Element text ~ char, One char (Bwd m char)) => One char (Bwd (BP (FileT ('Just 'Backward) 'ReadWriteMode text m)) char) where one = onePutChar
instance (MonadFilePutChar 'AppendMode char, MonadIO m, Element text ~ char, One char (Bwd m char)) => One char (Bwd (BP (FileT 'Nothing 'AppendMode text m)) char) where one = onePutChar
onePutChar ::
  ( One char (Bwd m char)
  , MonadIO m
  , MonadFilePutChar mode char
  ) => Bwd (BP (FileT d mode text m)) char char
onePutChar = Bwd \c -> BP $ lift (runBwd one c) <* putChar c

--instance BiN text (Fwd (BP (FileT 'Nothing 'ReadMode text m)) text) where
--  biN = _

instance
  ( MonoTraversable text
  , Show text
  , Eq char
  , MonadFail m
  , MonadIO m
  , Try m
  , OnError m
  , One char (Fwd (BP (FileT 'Nothing 'ReadMode text m)) char)
  , char ~ Element text
  , StripPrefix text (Fwd m text)
  ) => StripPrefix text (Fwd (BP (FileT 'Nothing 'ReadMode text m)) u) where
  stripPrefix = stripPrefixFile @text
instance
  ( MonoTraversable text
  , Show text
  , Eq char
  , MonadFail m
  , MonadIO m
  , Try m
  , OnError m
  , MonadState s m
  , One char (Fwd (BP (FileT ('Just 'Forward) 'ReadWriteMode text m)) char)
  , char ~ Element text
  , StripPrefix text (Fwd m text)
  ) => StripPrefix text (Fwd (BP (FileT ('Just 'Forward) 'ReadWriteMode text m)) u) where
  stripPrefix = stripPrefixFile @text

stripPrefixFile :: forall text seq t m u char.
  ( MonadFail (t m)
  , Show seq
  , Eq char
  , One char (Fwd (BP (t m)) char)
  , char ~ Element seq
  , MonoTraversable seq
  , Try (BP (t m))
  , MonadTrans t
  , StripPrefix seq (Fwd m text)
  , Monad m
  ) => seq -> Fwd (BP (t m)) u ()
stripPrefixFile prefix = do
  Fwd $ BP $ lift $ runFwd @_ @text $ stripPrefix prefix
  try $ for_ prefix \x -> do
    y <- one `uponConst` x
    when (x /= y) $ fail $ "Could not match prefix: " <> show prefix

instance
  ( StripPrefix text (Bwd m text)
  , MonadFilePutStr 'WriteMode text
  , MonadIO m
  ) => StripPrefix text (Bwd (BP (FileT 'Nothing 'WriteMode text m)) u) where
  stripPrefix prefix = Bwd $ const $ BP do
    lift $ runBwd (stripPrefix prefix) prefix
    putStr prefix

instance
  ( StripPrefix text (Bwd m text)
  , MonadFilePutStr 'ReadWriteMode text
  , MonadIO m
  ) => StripPrefix text (Bwd (BP (FileT ('Just 'Backward) 'ReadWriteMode text m)) u) where
  stripPrefix prefix = Bwd $ const $ BP do
    lift $ runBwd (stripPrefix prefix) prefix
    putStr prefix

instance
  ( StripPrefix text (Bwd m text)
  , MonadFilePutStr 'AppendMode text
  , MonadIO m
  ) => StripPrefix text (Bwd (BP (FileT 'Nothing 'AppendMode text m)) u) where
  stripPrefix prefix = Bwd $ const $ BP do
    lift $ runBwd (stripPrefix prefix) prefix
    putStr prefix

instance (MonadIO m, MonadMask m, Peek m) => Peek (BP (FileT 'Nothing 'ReadMode text m)) where
  peek (BP x) = BP $ do
    p <- liftIO . hGetPosn =<< askHandle
    finally (liftThrough peek x) $ liftIO $ hSetPosn p

instance (MonadIO m, MonadMask m, Peek m) => Peek (BP (FileT ('Just 'Forward) 'ReadWriteMode text m)) where
  peek (BP x) = BP $ do
    p <- liftIO . hGetPosn =<< askHandle
    finally (liftThrough peek x) $ liftIO $ hSetPosn p

instance (MonadIO m, Try m, OnError m) => Try (BP (FileT 'Nothing 'ReadMode text m)) where
  try (BP x) = BP $ do
    p <- liftIO . hGetPosn =<< askHandle
    onError (liftThrough try x) $ liftIO $ hSetPosn p

instance (MonadIO m, Try m, OnError m) => Try (BP (FileT ('Just 'Forward) 'ReadWriteMode text m)) where
  try (BP x) = BP $ do
    p <- liftIO . hGetPosn =<< askHandle
    onError (liftThrough try x) $ liftIO $ hSetPosn p

instance (MonadFilePutStr 'AppendMode text, MonadFail m, OnError m, MonadIO m, Try m, Monoid text) => Try (BP (FileT 'Nothing 'AppendMode text m)) where
  try (BP x) = BP $ onError
    do
      push mempty
      y <- liftThrough try x
      z <- pop
      onError
        do
          z' <- pop
          push $ z' <> z
        do
          putStr z
      return y
    do
      pop

instance OnError m => OnError (FileT d mode text m) where
  onError (FileT x) (FileT y) = FileT \fp h s -> onError (x fp h s) (y fp h s)
