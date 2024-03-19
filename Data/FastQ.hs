{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Data.FastQ where

import Control.Monad
import Control.Applicative
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import Data.ByteString.Lazy.Internal (defaultChunkSize)
import Data.Function
import Data.Streaming.Zlib
import Data.Word (Word8)
import Lens.Micro
import Lens.Micro.TH
import Streamly.Data.Array (Array)
import qualified Streamly.Data.Fold as F
import Streamly.Data.Stream (Stream)
import qualified Streamly.Data.Stream as S
import qualified Streamly.External.ByteString as SB
import qualified Streamly.Internal.Data.Stream.Chunked as AS
import Streamly.Internal.Data.Stream.StreamD.Type (Step (..))
import Streamly.Internal.Data.Unfold.Type (Unfold (..))
import System.IO
import Prelude hiding (Read)

data Read = Read
  { _qual :: ByteString,
    _nucs :: ByteString,
    _header :: ByteString
  }
  deriving (Eq, Show, Ord)

makeLenses ''Read

type ReadPair = (Read, Read)

gzipWindow :: WindowBits
gzipWindow = WindowBits 31

parse :: (MonadIO m) => FilePath -> FilePath -> Stream m ReadPair
parse l r =
  S.zipWith
    (liftA2 (,))
    (S.unfold streamBGZFile l & AS.splitOn 10 & fmap SB.fromArray & S.foldMany parseRead)
    (S.unfold streamBGZFile r & AS.splitOn 10 & fmap SB.fromArray & S.foldMany parseRead)
    & S.catMaybes
  where
    streamBGZFile :: (MonadIO m) => Unfold m FilePath (Array Word8)
    streamBGZFile = Unfold step seed
      where
        seed path = liftIO $ do
          h <- openFile path ReadMode
          i <- initInflate gzipWindow
          pure $ Just (h, i, Nothing)
        step Nothing = pure Stop
        step (Just (h, i, Nothing)) = liftIO $ do
          complete <- isCompleteInflate i
          (frag, unused, i) <-
            if complete
              then do
                f <- finishInflate i
                u <- getUnusedInflate i
                (f,u,) <$> initInflate gzipWindow
              else pure ("", "", i)
          if unused == ""
            then do
              chunk <- B.hGet h defaultChunkSize
              if chunk == B.empty
                then do
                  hClose h
                  str <- finishInflate i
                  pure $ Yield (SB.toArray $ frag <> str) Nothing
                else do
                  popper <- feedInflate i $ chunk
                  pure $ Yield (SB.toArray frag) (Just (h, i, Just popper))
            else do
              popper <- feedInflate i $ unused
              pure $ Yield (SB.toArray frag) (Just (h, i, Just popper))
        step (Just (h, i, Just popper)) = liftIO $ do
          popper >>= \case
            PRNext str -> pure $ Yield (SB.toArray str) (Just (h, i, Just popper))
            PRDone -> pure $ Yield (SB.toArray "") (Just (h, i, Nothing))
            PRError e -> error $ "parse:" <> show e

    parseRead =
      (liftA4 (\(64 :. hdr) seq "+" qual -> Read qual seq hdr))
        <$> F.one
        <*> F.one
        <*> F.one
        <*> F.one

    liftA4 fn a b c d = fn <$> a <*> b <*> c <*> d

pattern (:.) :: Word8 -> ByteString -> ByteString
pattern a :. b <- (B.uncons -> Just (a, b))

unparse :: FilePath -> FilePath -> Stream IO ReadPair -> IO ()
unparse l r str = do
  lh <- openFile l WriteMode
  rh <- openFile r WriteMode
  ld <- initDeflate 0 gzipWindow
  rd <- initDeflate 0 gzipWindow
  fmap unparse' str & S.fold (F.drainMapM $ writeFiles lh rh ld rd)
  flush rd rh
  flush ld lh
  hClose rh
  hClose lh
  where
    writeFiles l r ld rd (a, b) = do
      putCompressed ld l a
      putCompressed rd r b

    putCompressed d h chunk = do
      popper <- feedDeflate d chunk
      writePopper h popper

    flush d h = finishDeflate d & writePopper h

    writePopper h p =
      p >>= \case
        PRNext str -> do
          B.hPut h str
          writePopper h p
        PRDone -> pure ()
        PRError e -> error $ "parse:" <> show e

    unparse' :: ReadPair -> (ByteString, ByteString)
    unparse' read =
      ( BC.unlines ["@" <> read ^. _1 . header, read ^. _1 . nucs, "+", read ^. _1 . qual],
        BC.unlines ["@" <> read ^. _2 . header, read ^. _2 . nucs, "+", read ^. _2 . qual]
      )
