{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Dedumi where

import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.Cuckoo
import Data.FastQ
import Data.Function
import GHC.Prim (RealWorld)
import GHC.TypeLits
import Lens.Micro
import Options.Generic
import qualified Streamly.Data.Stream as S

newtype NoLabel a = NoLabel {unNoLabel :: a} deriving (Generic, Show)

data Options w = Options
  { umiLength :: w ::: Natural <?> "length of UMI prefix" <!> "8",
    extraHashBases :: w ::: Natural <?> "extra hash bases to use for location proxy" <!> "4",
    filterSize :: w ::: Natural <?> "Cuckoo filter size" <!> "200000000",
    input1 :: w ::: NoLabel FilePath <?> "input fastq 1 path",
    input2 :: w ::: NoLabel FilePath <?> "input fastq 2 path",
    output1 :: w ::: NoLabel FilePath <?> "output fastq 1 path",
    output2 :: w ::: NoLabel FilePath <?> "output fastq 2 path"
  }
  deriving (Generic)

instance ParseFields a => ParseRecord (NoLabel a)

instance ParseFields a => ParseFields (NoLabel a) where
  parseFields msg _ _ def = fmap NoLabel (parseFields msg Nothing Nothing def)

instance ParseRecord (Options Wrapped)

instance CuckooFilterHash ByteString where
  cuckooHash (Salt s) = saltedFnv1aByteString s
  cuckooFingerprint (Salt s) = saltedSipHashByteString s
  {-# INLINE cuckooHash #-}
  {-# INLINE cuckooFingerprint #-}

trim :: Int -> ReadPair -> ReadPair
trim sz x =
  x
    & _1 . nucs %~ B.drop sz
    & _2 . nucs %~ B.drop sz
    & _1 . qual %~ B.drop sz
    & _2 . qual %~ B.drop sz

insert' :: (KnownNat b, KnownNat f) => Int -> CuckooFilter RealWorld b f ByteString -> ReadPair -> IO Bool
insert' sz f x =
  let y = B.take sz (x ^. _1 . nucs) <> B.take sz (x ^. _2 . nucs)
   in member f y >>= \case
        True -> pure False
        False ->
          insert f y >>= \case
            True -> pure True
            False -> error "filter full"

go opts = do
  f <- newCuckooFilter @4 @13 @ByteString 0 (filterSize opts)

  parse (unNoLabel $ input1 opts) (unNoLabel $ input2 opts)
    & S.filterM (insert' (fromIntegral $ umiLength opts + extraHashBases opts) f)
    & fmap (trim . fromIntegral $ umiLength opts)
    & unparse (unNoLabel $ output1 opts) (unNoLabel $ output2 opts)
