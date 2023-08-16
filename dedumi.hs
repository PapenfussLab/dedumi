{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Main where

import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.Cuckoo
import Data.FastQ
import Data.Function
import GHC.Prim (RealWorld)
import GHC.TypeLits
import Lens.Micro
import qualified Streamly.Data.Stream as S
import System.Environment

instance CuckooFilterHash ByteString where
  cuckooHash (Salt s) = saltedFnv1aByteString s
  cuckooFingerprint (Salt s) = saltedSipHashByteString s
  {-# INLINE cuckooHash #-}
  {-# INLINE cuckooFingerprint #-}

umiLength :: Int
umiLength = 8

extraHashBases :: Int
extraHashBases = 4

trim :: ReadPair -> ReadPair
trim x =
  x
    & _1 . nucs %~ B.drop umiLength
    & _2 . nucs %~ B.drop umiLength
    & _1 . qual %~ B.drop umiLength
    & _2 . qual %~ B.drop umiLength

insert' :: (KnownNat b, KnownNat f) => CuckooFilter RealWorld b f ByteString -> ReadPair -> IO Bool
insert' f x =
  let y = B.take (umiLength + extraHashBases) (x ^. _1 . nucs) <> B.take (umiLength + extraHashBases) (x ^. _2 . nucs)
   in member f y >>= \case
        True -> pure True
        False ->
          insert f y >>= \case
            True -> pure False
            False -> error "filter full"

main :: IO ()
main = do
  [p1, p2, p3, p4] <- getArgs

  f <- newCuckooFilter @4 @13 @ByteString 0 200_000_000

  parse p1 p2
    & S.filterM (insert' f)
    & fmap trim
    & unparse p3 p4
