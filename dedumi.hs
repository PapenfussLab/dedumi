{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.Cuckoo
import Data.FastQ
import Data.Function
import Data.Typeable
import Lens.Micro
import qualified Streamly.Data.Stream as S
import System.Environment
import Prelude hiding (reads)

instance CuckooFilterHash ByteString where
  cuckooHash (Salt s) = saltedFnv1aByteString s
  cuckooFingerprint (Salt s) = saltedSipHashByteString s
  {-# INLINE cuckooHash #-}
  {-# INLINE cuckooFingerprint #-}

umiLength = 8

trim x =
  x
    & reads . _1 . nucs %~ B.drop umiLength
    & reads . _2 . nucs %~ B.drop umiLength
    & reads . _1 . qual %~ B.drop umiLength
    & reads . _2 . qual %~ B.drop umiLength

insert' f x =
  let y = B.take umiLength (x ^. reads . _1 . nucs) <> B.take umiLength (x ^. reads . _2 . nucs)
   in member f y >>= \case
        True -> pure True
        False ->
          insert f y >>= \case
            True -> pure False
            False -> error "filter full"

main = do
  [p1, p2, p3, p4] <- getArgs

  f <- newCuckooFilter @4 @13 @ByteString 0 20_000_000

  parse p1 p2
    & S.filterM (insert' f)
    & fmap trim
    & unparse p3 p4

  pure ()
