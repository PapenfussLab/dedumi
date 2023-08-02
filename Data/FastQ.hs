{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

module Data.FastQ where

import Codec.Compression.GZip
import Data.Bifunctor (bimap)
import Control.Monad
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Internal as BSL
import Data.Function
import Data.Void
import Data.Word (Word8)
import Debug.Trace
import Lens.Micro
import Lens.Micro.TH
import Streamly.Data.Array (Array)
import Streamly.Data.Fold as F
import Streamly.Data.Stream (Stream)
import qualified Streamly.Data.Stream as S
import qualified Streamly.External.ByteString as SB
import Streamly.Internal.Data.Stream.Chunked as AS
import Streamly.Internal.Data.Stream.StreamD.Type (Step (..))
import Streamly.Internal.Data.Unfold.Type (Unfold (..))
import qualified Streamly.Internal.FileSystem.File as File
import System.IO
import System.IO.Unsafe (unsafeInterleaveIO)
import Prelude hiding (Read, reads)

data Read = Read
  { _qual :: ByteString,
    _nucs :: ByteString
  }
  deriving (Eq, Show, Ord)

makeLenses ''Read

data ReadPair = ReadPair
  { _header :: ByteString,
    _reads :: (Read, Read)
  }
  deriving (Eq, Show, Ord)

makeLenses ''ReadPair

parse :: MonadIO m => FilePath -> FilePath -> Stream m ReadPair
parse l r =
  S.zipWith
    parseEntry
    (S.unfold streamFile l & AS.splitOn 64)
    (S.unfold streamFile r & AS.splitOn 64)
  where
    streamFile :: MonadIO m => Unfold m FilePath (Array Word8)
    streamFile = Unfold step seed
      where
        seed = pure $ BSL.tail . decompress <$> liftIO (BSL.readFile r)
        step (BSL.Chunk bs bl) = pure $ Yield (SB.toArray bs) bl
        step BSL.Empty = pure Stop

    parseEntry l r =
      let [hdr, seq, "+", qual] = SB.fromArray l & BC.lines
          [_, seq', "+", qual'] = SB.fromArray r & BC.lines
       in ReadPair hdr (Read qual seq, Read qual' seq')

unparse :: FilePath -> FilePath -> Stream IO ReadPair -> IO ()
unparse l r str = do
  (compress -> left, compress -> right) <- fmap unparse' str & toLazyBS
  lh <- openFile l WriteMode
  rh <- openFile r WriteMode
  writeFiles lh rh left right
  where
    writeFiles l r (BSL.Chunk a as) (BSL.Chunk b bs) = do
      B.hPut l a
      B.hPut r b
      writeFiles l r as bs
    writeFiles l _ a BSL.Empty = BSL.hPut l a
    writeFiles _ r BSL.Empty b = BSL.hPut r b

    unparse' read =
      ( BC.unlines [read ^. header, read ^. reads . _1 . nucs, "+", read ^. reads . _1 . qual],
        BC.unlines [read ^. header, read ^. reads . _2 . nucs, "+", read ^. reads . _2 . qual]
      )

    toLazyBS :: Stream IO (ByteString, ByteString) -> IO (BSL.ByteString, BSL.ByteString)
    toLazyBS =
      S.foldrM (\(l, r) b -> bimap (BSL.chunk ("@" <> l)) (BSL.chunk ("@" <> r)) <$> unsafeInterleaveIO b) (pure (BSL.Empty, BSL.Empty))
