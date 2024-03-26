{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import qualified Data.ByteString.Char8 as B
import Data.FastQ
import Data.Function
import qualified Streamly.Data.Fold as F
import qualified Streamly.Data.Stream as S
import qualified Streamly.Data.Unfold as U
import System.IO.Temp
import Test.QuickCheck
import Test.QuickCheck.Monadic
import Prelude hiding (Read)

instance Arbitrary Read where
  arbitrary = Read <$> genStr <*> genStr <*> genStr
    where
      genBS = fmap B.pack . listOf . elements
      genStr = genBS $ [' ' .. '~']

main :: IO ()
main = quickCheckWith stdArgs {maxSuccess = 10000} $ \rp -> monadicIO $ do
  rp' <- run $ withSystemTempDirectory "dedumi-test" $ \root -> do
    let a = root <> "/a"
        b = root <> "/b"
        a' = root <> "/a2"
        b' = root <> "/b2"
    S.unfold U.fromList rp & unparse a b
    cnt <- B.readFile a
    B.writeFile a' $ cnt <> cnt
    cnt <- B.readFile b
    B.writeFile b' $ cnt <> cnt
    parse a' b' & S.fold F.toList
  assert $ rp <> rp == rp'
