{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Monad
import Criterion
import Criterion.Main
import qualified Data.ByteString.Char8 as B
import Data.FastQ
import Data.Function
import qualified Streamly.Data.Stream as S
import qualified Streamly.Data.Unfold as U
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen
import Prelude hiding (Read)
import System.IO.Temp
import Options.Generic
import qualified Dedumi as D
import Data.Maybe
import qualified Data.Text as T

instance Arbitrary Read where
  arbitrary = Read <$> genStr <*> genStr <*> genStr
    where
      genBS = fmap B.pack . listOf1 . elements
      genStr = genBS $ [' ' .. '~']

--sizes = [2 ^ i | i <- [11 .. 19]]
sizes = [2*8192]

setupEnv root size = do
    let fileroot = root <> "/" <> show size
        a = fileroot <> "a"
        b = fileroot <> "b"
    rp <- replicateM size $ generate $ arbitrary
    S.unfold U.fromList rp & unparse a b
    pure (a, b)

main :: IO ()
main = withSystemTempDirectory "dedumi-bench" $ \tmpdir ->
  defaultMain
    [ bgroup "main" $ map (\s -> env (setupEnv tmpdir s) $ \ ~(a,b) -> bench (show s) (run (T.pack a) (T.pack b))) sizes
    ]

run fq1 fq2 = nfAppIO D.go $ fromJust $ unwrapRecordPure [fq1, fq2, "/dev/null", "/dev/null"]
