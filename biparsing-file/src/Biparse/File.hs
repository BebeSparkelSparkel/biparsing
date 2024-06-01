{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-missing-deriving-strategies #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
module Biparse.File (
BiparserEasy,
IsoEasy,
ConstEasy,
decodeFileEasy,
encodeToFileEasy,
FileRWST,
Biparser,
Iso,
Const,
BinaryFile,
BinaryPosition(BinaryPosition),
TextFile,
TextPosition(TextPosition),
decodeBinaryFile,
decodeTextFile,
encodeToBinaryFile,
encodeToTextFile,
) where

import Biparse.Biparser (pattern Biparser, GetState, GetSubState(getSubState), SubState, SuperState, InitSuperState(fromSubState), SuperArg, ConvertSequence(convertSequence), ConvertElement(convertElement), UpdateStateWithSubState(updateSubStateContext), UpdateStateWithElement(updateElementContext))
import Biparse.Biparser.StateReaderWriter (BackwardC(runBackwardT), BackwardArg, BackwardArgC, BackwardT)
import Biparse.Biparser.StateReaderWriter qualified as SRW
import Biparse.Context.Index (IndexContext, IndexPosition(IndexPosition), startIndex)
import Biparse.Text.Context.LineColumn (Position(Position), UnixLC, startLineColumn')
import Control.Monad.FileT (FileT(FileT), Open(open), withFile)
import Control.Monad.RWS.CPS (runRWST)
import Control.Monad.Reader (MonadReader(ask))
import Control.Monad.State (StateT)
import Control.Monad.State.Class (MonadState(get,put))
import Control.Monad.StateError (runStateErrorT)
import Control.Monad.Trans.Class (MonadTrans(lift))
import Control.Monad.Trans.RWS.CPS (RWST)
import Control.Monad.Trans.State.Selectable (StateTransformer)
import Control.Monad.Writer.Class (MonadWriter(tell))
import Data.ByteString.Builder qualified as BB
import Data.ByteString.Lazy qualified as BL
import Data.Coerce (coerce)
import Data.MonoTraversable (MonoPointed, opoint, MonoFoldable)
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Builder qualified as TB
import Data.Text.Lazy.IO qualified as TL
import Lens.Micro ((.~), _3)
import System.IO qualified as S

-- * Easy Types and Functions
--
-- | Try using these types and functions before using the more general ones in the next section.

type BiparserEasy ss m = Biparser (TextOrBinaryFile ss) ss m () ()
type IsoEasy ss m v = BiparserEasy ss m v v
type ConstEasy ss m u = BiparserEasy ss m u ()

type TextOrBinaryFile :: Type -> Type
type family TextOrBinaryFile ss
type instance TextOrBinaryFile String = TextFile
type instance TextOrBinaryFile TL.Text = TextFile
type instance TextOrBinaryFile BL.ByteString = BinaryFile

decodeFileEasy :: forall ss m u v.
  ( SuperArg (SuperState (TextOrBinaryFile ss) ss) ~ FilePath
  , InitSuperState (TextOrBinaryFile ss) ss
  , MonadUnliftIO m
  , Open ss
  , GetContents ss
  )
  => FilePath
  -> BiparserEasy ss m u v
  -> m v
decodeFileEasy fp bp = fst <$> decodeFile @ss fp bp

encodeToFileEasy :: forall ss m u v.
  ( Functor m
  , BackwardArgC (TextOrBinaryFile ss) ~ Handle
  , BackwardC (TextOrBinaryFile ss) m () (FileAssociatedWriter ss) ()
  , Open (FileAssociatedWriter ss)
  , MonadUnliftIO m
  , SubState (SuperState (TextOrBinaryFile ss) ss) ~ ss
  )
  => FilePath
  -> u
  -> BiparserEasy ss m u v
  -> m v
encodeToFileEasy fp u bp = fst <$> encodeToFile @ss fp () () u bp

-- * More General Types and Functions
--
-- | Try using these before using the more general types and functions defined in Biparse.Biparser.StateReaderWriter and Biparse.Biparser

newtype FileRWST r w s m a = FileRWST (FileT w (RWST r () s m) a)
  deriving (Functor, Applicative, Monad, MonadFail)
deriving instance (Alt m, Monad m) => Alt (FileRWST r w s m)
deriving instance Monad m => MonadReader r (FileRWST r w s m)
deriving instance (MonadWriter w (FileT w (RWST r () s m)), Monoid w, Monad m) => MonadWriter w (FileRWST r w s m)
deriving instance Monad m => MonadState s (FileRWST r w s m)
instance MonadTrans (FileRWST r w s) where lift = FileRWST . lift . lift
type instance BackwardArg FileRWST = Handle


runFileRWST :: (Functor m, Monoid w) => FileRWST r w s m a -> Handle -> r -> s -> m (a, s, w)
runFileRWST (FileRWST (FileT f)) h r s = runRWST (f h) r s <&> _3 .~ mempty
  

type Biparser c ss m r ws = SRW.Biparser c (SuperState c ss) m m r (FileAssociatedWriter ss) ws
type Iso c m ss r ws v = Biparser c ss m r ws v v
type Const c ss m r ws u = Biparser c ss m r ws u ()

-- * File Types

-- ** Binary File
data BinaryFile

data BinaryPosition = BinaryPosition' FilePath (IndexPosition LazyByteString) deriving (Show, Eq)
pattern BinaryPosition :: FilePath -> Index LazyByteString -> LazyByteString -> BinaryPosition
pattern BinaryPosition fp i ss = BinaryPosition' fp (IndexPosition i ss)

instance GetSubState BinaryPosition where
  type SubState BinaryPosition = LazyByteString
  getSubState (BinaryPosition' _ x) = getSubState x

instance InitSuperState BinaryFile LazyByteString where
  type SuperState BinaryFile LazyByteString = BinaryPosition
  fromSubState fp = BinaryPosition' fp . startIndex
type instance SuperArg BinaryPosition = FilePath

instance UpdateStateWithSubState BinaryFile BinaryPosition where
  updateSubStateContext (BinaryPosition' fp ip) ss ss' = BinaryPosition' fp $ updateSubStateContext @IndexContext ip ss ss'

instance Monad m => ConvertSequence BinaryFile LazyByteString BB.Builder m where
  convertSequence = return . BB.lazyByteString

type instance BackwardT BinaryFile = FileRWST

instance (MonadWriter w (FileT w (RWST r () s m)), Monad m) => BackwardC BinaryFile m r w s where
  backwardT = backwardT'
  runBackwardT = runFileRWST

backwardT' ::
  ( MonadState s (t m)
  , MonadWriter w (t m)
  , Monad m
  , MonadTrans t
  , MonadReader r (t m)
  ) => (r -> s -> m (b, s, w)) -> t m b
backwardT' f = do
  r <- ask
  s <- get
  (a,s,w) <- lift $ f r s
  tell w
  put s
  return a

-- ** Text File

data TextFile

newtype TextPosition text = TextPosition' (Position FilePath text) deriving (Show, Eq)
pattern TextPosition :: FilePath -> Int -> Int -> text -> TextPosition text
pattern TextPosition fp l c text = TextPosition' (Position fp l c text)

type instance SuperArg (TextPosition _) = FilePath

instance InitSuperState TextFile text where
  type SuperState TextFile text = TextPosition text
  fromSubState fp text = TextPosition' $ startLineColumn' fp text

instance GetSubState (TextPosition text) where
  type SubState (TextPosition text) = text
  getSubState (TextPosition' p) = getSubState p

instance (Eq (Element text), IsChar (Element text)) => UpdateStateWithElement TextFile (TextPosition text) where
  updateElementContext = coerce $ updateElementContext @UnixLC @(Position FilePath text)

instance (MonoFoldable text, Eq (Element text), IsChar (Element text)) => UpdateStateWithSubState TextFile (TextPosition text) where
  updateSubStateContext = coerce $ updateSubStateContext @UnixLC @(Position FilePath text)

instance (Applicative m, MonoPointed w, IsChar (Element w)) => ConvertElement TextFile Char w m where
  convertElement = pure . opoint . fromChar

instance (MonadWriter w (FileT w (RWST r () s m)), Monad m) => BackwardC TextFile m r w s where
  backwardT = backwardT'
  runBackwardT = runFileRWST

type instance BackwardT TextFile = FileRWST

type instance StateTransformer TextFile = StateT

instance Monad m => ConvertSequence TextFile LazyText TB.Builder m where
  convertSequence = return . TB.fromLazyText

instance Monad m => ConvertSequence TextFile String String m where
  convertSequence = return

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
  )
  => FilePath
  -> Biparser BinaryFile LazyByteString m r ws u v
  -> m (v, BinaryPosition)
decodeBinaryFile = decodeFile @LazyByteString

decodeTextFile :: forall m r ws u v.
  ( MonadUnliftIO m
  )
  => FilePath
  -> Biparser TextFile LazyText m r ws u v
  -> m (v, TextPosition LazyText)
decodeTextFile = decodeFile @LazyText

decodeFile :: forall ss c m r ws u v.
  ( SuperArg (SuperState c ss) ~ FilePath
  , InitSuperState c ss
  , GetContents ss
  , MonadUnliftIO m
  , Open ss
  )
  => FilePath
  -> Biparser c ss m r ws u v
  -> m (v, SuperState c ss)
decodeFile  fp (Biparser fw _) = liftIO (open @ss fp ReadMode) >>= \h -> do
  bs <- liftIO $ hGetContents @ss h
  runStateErrorT fw (fromSubState @c fp bs)

encodeToBinaryFile :: forall m r ws u v.
  ( BackwardC BinaryFile m r BB.Builder ws
  , MonadUnliftIO m
  )
  => FilePath
  -> r
  -> ws
  -> u
  -> Biparser BinaryFile LazyByteString m r ws u v
  -> m (v, ws)
encodeToBinaryFile = encodeToFile @LazyByteString

encodeToTextFile :: forall m r ws u v.
  ( BackwardC TextFile m r TB.Builder ws
  , MonadUnliftIO m
  )
  => FilePath
  -> r
  -> ws
  -> u
  -> Biparser TextFile LazyText m r ws u v
  -> m (v, ws)
encodeToTextFile = encodeToFile @LazyText

encodeToFile :: forall ss c m r ws u v w.
  ( BackwardArgC c ~ Handle
  , BackwardC c m r (FileAssociatedWriter ss) ws
  , Open w
  , MonadUnliftIO m
  , FileAssociatedWriter ss ~ w
  , SubState (GetState (Biparser c ss m r ws u v)) ~ ss
  )
  => FilePath
  -> r
  -> ws
  -> u
  -> Biparser c ss m r ws u v
  -> m (v, ws)
encodeToFile fp r ws u (Biparser _ bw) = withFile @w fp WriteMode \h -> runBackwardT @c (bw u) h r ws <&> \(v,ws,_) -> (v,ws)

