{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Data.FastQ where

import Codec.Zlib
import Control.Monad
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Bifunctor (bimap)
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Internal as BSL
import Data.Function
import Data.Word (Word8)
import Lens.Micro
import Lens.Micro.TH
import Streamly.Data.Array (Array)
import Streamly.Data.Stream (Stream)
import qualified Streamly.Data.Stream as S
import qualified Streamly.External.ByteString as SB
import Streamly.Internal.Data.Stream.Chunked as AS
import Streamly.Internal.Data.Stream.StreamD.Type (Step (..))
import Streamly.Internal.Data.Unfold.Type (Unfold (..))
import System.IO
import System.IO.Unsafe (unsafeInterleaveIO)
import Prelude hiding (Read)

data Read = Read
  { _qual :: ByteString,
    _nucs :: ByteString,
    _header :: ByteString
  }
  deriving (Eq, Show, Ord)

makeLenses ''Read

type ReadPair = (Read, Read)

gzipWindow = WindowBits 31

parse :: MonadIO m => FilePath -> FilePath -> Stream m ReadPair
parse l r =
  S.zipWith
    parseEntry
    (S.unfold streamFile l & AS.splitOn 64 & S.drop 1)
    (S.unfold streamFile r & AS.splitOn 64 & S.drop 1)
  where
    streamFile :: MonadIO m => Unfold m FilePath (Array Word8)
    streamFile = Unfold step seed
      where
        seed path = liftIO $ do
          h <- openFile path ReadMode
          i <- initInflate gzipWindow
          pure $ Just (h, i, Nothing)
        step Nothing = pure Stop
        step (Just (h, i, Nothing)) = liftIO $ step' h i
        step (Just (h, i, Just popper)) = liftIO $ do
          popper >>= \case
            Just str -> pure $ Yield (SB.toArray str) (Just (h, i, Just popper))
            Nothing -> step' h i

        step' h i = do
          chunk <- B.hGet h BSL.defaultChunkSize
          if chunk == B.empty
            then do
              str <- finishInflate i
              pure $ Yield (SB.toArray str) Nothing
            else do
              p <- feedInflate i chunk
              p >>= \case
                Just str -> pure $ Yield (SB.toArray str) (Just (h, i, Just p))
                Nothing -> step' h i

    parseEntry l r =
      let [hdr, seq, "+", qual] = SB.fromArray l & BC.lines
          [hdr', seq', "+", qual'] = SB.fromArray r & BC.lines
       in (Read qual seq hdr, Read qual' seq' hdr')

unparse :: FilePath -> FilePath -> Stream IO ReadPair -> IO ()
unparse l r str = do
  lh <- openFile l WriteMode
  rh <- openFile r WriteMode
  ld <- initDeflate 0 gzipWindow
  rd <- initDeflate 0 gzipWindow
  fmap unparse' str & toLazyBS >>= uncurry (writeFiles lh rh ld rd)
  where
    writeFiles l r ld rd (BSL.Chunk a as) (BSL.Chunk b bs) = do
      putCompressed ld l a
      putCompressed rd r b
      writeFiles l r ld rd as bs
    writeFiles l r ld rd BSL.Empty BSL.Empty = do
      flush rd r
      flush ld l
    writeFiles _ _ _ _ _ _ = error "unparse: stream mismatch"

    putCompressed d h chunk = do
      popper <- feedDeflate d chunk
      writePopper popper h

    flush d h =
      finishDeflate d >>= \case
        Just rest -> B.hPut h rest
        Nothing -> pure ()

    writePopper p h =
      p >>= \case
        Just str -> do
          B.hPut h str
          writePopper p h
        Nothing -> pure ()

    unparse' :: ReadPair -> (ByteString, ByteString)
    unparse' read =
      ( BC.unlines [read ^. _1 . header, read ^. _1 . nucs, "+", read ^. _1 . qual],
        BC.unlines [read ^. _2 . header, read ^. _2 . nucs, "+", read ^. _2 . qual]
      )

    toLazyBS :: Stream IO (ByteString, ByteString) -> IO (BSL.ByteString, BSL.ByteString)
    toLazyBS =
      S.foldrM (\(l, r) b -> bimap (BSL.chunk ("@" <> l)) (BSL.chunk ("@" <> r)) <$> unsafeInterleaveIO b) (pure (BSL.Empty, BSL.Empty))
