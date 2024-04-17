{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module Biparse.File
  ( Biparser
  , Iso
  , decodeFile
  , encodeFile
  ) where

import Biparse.Biparser (pattern Biparser, InitSuperState(SuperState,fromSubState), SuperArg, BackwardMonad)
import Control.Monad.FileT
import Data.Text.Lazy.IO qualified as TL
import Data.Text.Lazy.Builder qualified as TL
import System.IO qualified as S
import Data.ByteString.Lazy qualified as BL
import Data.ByteString.Builder qualified as BL
import Biparse.Text.Context.LineColumn
import Biparse.Context.Index
import Biparse.Biparser.StateReaderWriter qualified as SRW
import Biparse.Biparser.StateReaderWriter hiding (Biparser, Iso)
import Control.Monad.StateError
import Control.Monad.Reader
import Control.Monad.State.Class
import Control.Monad.Writer.Class
import Control.Monad.Trans
import Data.Monoid
import GHC.Err (undefined)


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

type Biparser c m ss r ws = SRW.Biparser c (SuperState c ss) m m r (AssociatedWriter ss) ws
type Iso c m ss r ws v = Biparser c m ss r ws v v

newtype TextPosition text = TextPosition (Position FilePath text) deriving (Show, Eq)

data BinaryFile
data TextFile

instance (Monad n, MonadWriter w (FileT r w s n), Monoid w) => BackwardC BinaryFile n r w s where
  type BackwardT BinaryFile = FileT
  backwardT f = do
    r <- ask
    s <- get
    (a, s', w) <- lift $ f r s
    tell w
    put s'
    return a
  runBackwardT x h r s = (\(a,s') -> (a,s',mempty)) <$> runFileT x h r s
type instance BackwardArg FileT = Handle

data BinaryPosition ss = BinaryPosition FilePath (IndexPosition ss)
deriving instance (Show ss, Show (Index ss)) => Show (BinaryPosition ss)
deriving instance (Eq ss, Eq (Index ss)) => Eq (BinaryPosition ss)
instance Num (Index ss) => InitSuperState BinaryFile ss where
  type SuperState BinaryFile ss = BinaryPosition ss
  fromSubState fp = BinaryPosition fp . startIndex
type instance SuperArg (BinaryPosition _) = FilePath

type StateType :: Type -> Type -> Type
type family StateType c where
  StateType LineColumnUnknownBreak = TextPosition
  StateType BinaryFile = BinaryPosition

type AssociatedWriter :: Type -> Type
type family AssociatedWriter ss where
  AssociatedWriter String = String
  AssociatedWriter LazyByteString = BL.Builder
  AssociatedWriter LazyText = TL.Builder

class GetContents ss where hGetContents :: Handle -> IO ss
instance GetContents String where hGetContents = S.hGetContents
instance GetContents LazyByteString where hGetContents = BL.hGetContents
instance GetContents LazyText where hGetContents = TL.hGetContents

decodeFile :: forall c m ss r ws u v aw.
  ( FilePath ~ SuperArg (SuperState c ss)
  , InitSuperState c ss
  , MonadUnliftIO m
  , GetContents ss
  , Open aw
  , Num (Index ss)
  , aw ~ AssociatedWriter ss
  )
  => FilePath
  -> Biparser c m ss r ws u v
  -> m v
decodeFile fp bp = withFile @aw fp ReadMode \h -> do
  ss <- liftIO $ hGetContents @ss h
  evalForward bp $ fromSubState @c fp ss

encodeFile :: forall c m ss r ws u v aw.
  ( Open aw
  , MonadUnliftIO m
  , BackwardC c m r aw ws
  , BackwardT c ~ FileT
  , aw ~ AssociatedWriter ss
  )
  => FilePath
  -> r
  -> ws
  -> u
  -> Biparser c m ss r ws u v
  -> m (v, ws)
encodeFile fp r ws u (Biparser _ bw) = withFile @aw fp ReadWriteMode \h ->
  (\(v,ws',_) -> (v,ws')) <$> runBackwardT @c (bw u) h r ws
