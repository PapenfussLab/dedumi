{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad
import Control.Monad.Trans
import Data.ByteString (ByteString)
import Data.ByteString qualified as B
import Data.ByteString.Char8 qualified as BC
import Data.Function
import Data.Streaming.Zlib
import Streamly.Data.Fold qualified as F
import Streamly.Data.Stream qualified as S
import System.Environment
import System.IO
import System.Random hiding (uniform)
import Prelude hiding (Read, read)

-- simulation parameters
readLength :: Int
readLength = 150

baseErrorRate :: Double
baseErrorRate = 0.00087 -- HiSeq X Ten

nreads :: Int
nreads = 1_000_000

-- Probability monad transformer
newtype P m a = P {runP :: StdGen -> m (a, StdGen)}

evalP :: (Monad m) => P m a -> StdGen -> m a
evalP (P f) g = fst <$> f g

instance (Monad m) => Monad (P m) where
  P f >>= g = P \q -> do
    (x, q') <- f q
    runP (g x) q'

instance (Monad m) => Functor (P m) where
  fmap = liftM

instance (Monad m) => Applicative (P m) where
  pure x = P \g -> pure (x, g)
  (<*>) = ap

instance (MonadIO m) => MonadIO (P m) where
  liftIO f = P \g -> (,g) <$> liftIO f

uniform :: Applicative m => P m Double
uniform = P (pure . random)

poisson :: Monad m => Double -> P m Int
poisson lambda = go 0 0
  where
    go i s
      | s >= 1 = pure (i - 1)
      | otherwise = do
          u <- uniform
          go (i + 1) (s - log u / lambda)


-- Reads/nucleotides and sampling them (with errors)
data Nuc = A | C | G | T
  deriving (Eq, Ord, Enum, Show, Bounded)

nuc :: (Monad m) => P m Nuc
nuc = toEnum . round . (* fromIntegral (fromEnum (maxBound :: Nuc))) <$> uniform

type Read = [Nuc]

data Pair a = Pair !a !a
  deriving (Eq, Ord, Show, Functor)

read :: (Monad m) => P m Read
read = replicateM readLength nuc

readPair :: (Monad m) => P m (Pair Read)
readPair = Pair <$> read <*> read

dup :: (Monad m) => Double -> Pair Read -> S.Stream (P m) (Pair Read)
dup p r = S.concatEffect do
  n <- poisson $ 1 / (1 - p) - 1
  pure $ S.replicate (1 + n) r

baseError :: (Monad m) => Double -> Read -> P m Read
baseError p = mapM error'
  where
    error' b = do
      q <- uniform
      if q < p
        then nuc
        else pure b

-- FastQ writer
data Entry = Entry ByteString (Pair Read) deriving (Eq, Ord, Show)

unparse :: (MonadIO m) => FilePath -> FilePath -> S.Stream m Entry -> m ()
unparse l r str = do
  lh <- liftIO $ openFile l WriteMode
  rh <- liftIO $ openFile r WriteMode
  ld <- liftIO $ initDeflate 0 gzipWindow
  rd <- liftIO $ initDeflate 0 gzipWindow
  fmap unparse' str & S.fold (F.drainMapM $ writeFiles lh rh ld rd)
  liftIO $ do
    flush rd rh
    flush ld lh
    hClose rh
    hClose lh
  where
    gzipWindow = WindowBits 31
    flush d h = finishDeflate d & writePopper h

    writeFiles l r ld rd (Pair a b) = do
      putCompressed ld l a
      putCompressed rd r b

    putCompressed d h chunk = liftIO $ do
      popper <- feedDeflate d chunk
      writePopper h popper

    writePopper h p =
      p >>= \case
        PRNext str -> do
          B.hPut h str
          writePopper h p
        PRDone -> pure ()
        PRError _ -> error "unparse: compression error"

    unparse' :: Entry -> Pair ByteString
    unparse' (Entry hdr (Pair l r)) = Pair (BC.unlines ["@" <> hdr, BC.pack $ concatMap show l, "+", qual]) (BC.unlines ["@" <> hdr, BC.pack $ concatMap show r, "+", qual])

    qual = BC.pack $ replicate readLength '~'

main :: IO ()
main = do
  [out1, out2] <- getArgs
  q <- getStdGen
  S.repeatM readPair -- Generate random reads
    & S.concatMap (dup 0.1) -- Duplicate reads
    & S.mapM (\(Pair l r) -> Pair <$> baseError baseErrorRate l <*> baseError baseErrorRate r) -- Introduce base errors
    & S.take nreads
    & S.zipWith (\i rp -> Entry (BC.pack $ show i) rp) (S.fromList [0 :: Int ..])
    & unparse out1 out2
    & flip evalP q
