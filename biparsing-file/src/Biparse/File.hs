{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module Biparse.File
  ( Biparser
  , Iso
  , decodeBinaryFile
  , decodeTextFile
  , decodeFile
  , encodeFile
  ) where

import Biparse.Biparser hiding (Biparser, Iso)
import Biparse.Biparser (pattern Biparser)
import Biparse.Biparser.StateReaderWriter hiding (Biparser, Iso)
import Biparse.Biparser.StateReaderWriter qualified as SRW
import Biparse.Context.Index
import Biparse.Text.Context.LineColumn
import Control.Monad.FileT
import Control.Monad.RWS.CPS
import Control.Monad.Reader
import Control.Monad.State.Class
import Control.Monad.StateError
import Control.Monad.Trans
import Control.Monad.Writer.Class
import Data.ByteString.Builder qualified as BB
import Data.ByteString.Lazy qualified as BL
import Data.Monoid
import Data.Text.Lazy.Builder qualified as TB
import Data.Text.Lazy.IO qualified as TL
import System.IO qualified as S
import Control.DeepSeq (NFData, deepseq)


-- -- * Easy Types and Functions
-- --
-- -- | Try using these types and functions before using the more general ones in the next section.
-- 
-- type BiparserEasy ss = Biparser (TextOrBinary ss) ss () ()
-- type IsoEasy ss v = BiparserEasy v v
-- 
-- type TextOrBinary :: Type -> Type
-- type family TextOrBinary ss where
--   TextOrBinary String = LineColumnUnknownBreak
--   TextOrBinary LazyByteString = BinaryFile
--   TextOrBinary LazyText = LineColumnUnknownBreak
-- 
-- decodeFileEasy ::
--   (
--   )
--   => BiparserEasy ss u v
--   -> FilePath
--   -> m v
-- decodeFileEasy = decodeFile
-- 
-- encodeToFileEasy ::
--   (
--   )
--   => BiparserEasy ss u v
--   -> FilePath
--   -> u
--   -> m v
-- encodeToFileEasy = encodeToFile

-- * More General Types and Functions
--
-- | Try using these before using the more general types and functions defined in Biparse.Biparser.StateReaderWriter and Biparse.Biparser

type Biparser c ss m r ws = SRW.Biparser c (SuperState c ss) m m r (FileAssociatedWriter ss) ws
type Iso c m ss r ws v = Biparser c ss m r ws v v

newtype TextPosition text = TextPosition (Position FilePath text) deriving (Show, Eq)

newtype FileRWST r w s m a = FileRWST (FileT w m a) deriving (Functor, Applicative, Monad, MonadFail, MonadTrans)
deriving instance (MonadReader r  (FileT w m), Monad m) => MonadReader r (FileRWST r w s m)
deriving instance (MonadWriter w  (FileT w m), Monad m) => MonadWriter w (FileRWST r w s m)
deriving instance (MonadState s  (FileT w m), Monad m) => MonadState s (FileRWST r w s m)

-- * File Types

-- ** Binary File
data BinaryFile

instance MonadWriter BB.Builder (FileT BB.Builder (RWST r () s IO)) => BackwardC BinaryFile (RWST r () s IO) r BB.Builder s where
  backwardT f = do
    r <- ask
    s <- get
    (a, s', w) <- lift $ f r s
    tell w
    put s'
    return a
  runBackwardT (FileRWST x) h _ _ = runFileT x h >>= \a -> do
    s <- get
    return (a, s, mempty)
type instance BackwardT BinaryFile = FileRWST
type instance BackwardArg FileRWST = Handle

data BinaryPosition = BinaryPosition FilePath (IndexPosition LazyByteString) deriving (Show, Eq)

instance GetSubState BinaryPosition where
  type SubState BinaryPosition = LazyByteString
  getSubState (BinaryPosition _ x) = getSubState x

instance InitSuperState BinaryFile LazyByteString where
  type SuperState BinaryFile LazyByteString = BinaryPosition
  fromSubState fp = BinaryPosition fp . startIndex
type instance SuperArg BinaryPosition = FilePath

instance UpdateStateWithSubState BinaryFile BinaryPosition where
  updateSubStateContext (BinaryPosition fp ip) ss ss' = BinaryPosition fp $ updateSubStateContext @IndexContext ip ss ss'

instance Monad m => ConvertSequence BinaryFile LazyByteString BB.Builder m where
  convertSequence = return . BB.lazyByteString

-- ** Text File

data TextFile

instance MonadWriter TB.Builder (FileT TB.Builder (RWST r () s IO)) => BackwardC TextFile (RWST r () s IO) r TB.Builder s where
  backwardT f = do
    r <- ask
    s <- get
    (a, s', w) <- lift $ f r s
    tell w
    put s'
    return a
  runBackwardT (FileRWST x) h _ _ = runFileT x h >>= \a -> do
    s <- get
    return (a, s, mempty)
type instance BackwardT TextFile = FileRWST
type instance BackwardArg FileRWST = Handle

instance InitSuperState TextFile LazyText where
  type SuperState TextFile LazyText = Position FilePath LazyText
  fromSubState = startLineColumn'

-- File Associated Writer Type

type FileAssociatedWriter :: Type -> Type
type family FileAssociatedWriter ss where
  FileAssociatedWriter String = String
  FileAssociatedWriter LazyByteString = BB.Builder
  FileAssociatedWriter LazyText = TB.Builder

class GetContents ss where hGetContents :: Handle -> IO ss
instance GetContents String where hGetContents = S.hGetContents
instance GetContents LazyByteString where hGetContents = BL.hGetContents
instance GetContents LazyText where hGetContents = TL.hGetContents

decodeBinaryFile :: forall m r ws u v.
  ( MonadUnliftIO m
  , NFData v
  )
  => FilePath
  -> Biparser BinaryFile LazyByteString m r ws u v
  -> m v
decodeBinaryFile = decodeFile @LazyByteString

decodeTextFile :: forall m r ws u v.
  ( MonadUnliftIO m
  , NFData v
  )
  => FilePath
  -> Biparser TextFile LazyText m r ws u v
  -> m v
decodeTextFile = decodeFile @LazyText

decodeFile :: forall ss c m r ws u v aw.
  ( FilePath ~ SuperArg (SuperState c ss)
  , InitSuperState c ss
  , MonadUnliftIO m
  , GetContents ss
  , Open aw
  , NFData v
  , aw ~ FileAssociatedWriter ss
  )
  => FilePath
  -> Biparser c ss m r ws u v
  -> m v
decodeFile fp bp = withFile @aw fp ReadMode \h -> do
  ss <- liftIO $ hGetContents @ss h
  v <- evalForward bp $ fromSubState @c fp ss
  deepseq v $ return v

encodeFile :: forall ss c m r ws u v aw.
  ( Open aw
  , MonadUnliftIO m
  , BackwardC c m r aw ws
  , BackwardArgC c ~ Handle
  , aw ~ FileAssociatedWriter ss
  )
  => FilePath
  -> r
  -> ws
  -> u
  -> Biparser c ss m r ws u v
  -> m (v, ws)
encodeFile fp r ws u (Biparser _ bw) = withFile @aw fp WriteMode \h ->
  (\(v,ws,_) -> (v,ws)) <$> runBackwardT @c (bw u) h r ws

