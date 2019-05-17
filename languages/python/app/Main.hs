{-# LANGUAGE OverloadedStrings #-}
module Main where

import TreeSitter.Python

main :: IO ()
main = do
  parseByteString "" >>= print
