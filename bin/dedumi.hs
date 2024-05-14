{-# LANGUAGE OverloadedStrings #-}
module Main where

import Dedumi
import Options.Generic

main :: IO ()
main =  unwrapRecord "UMI deduplication" >>= go
