{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.ByteString as B
import qualified TreeSitter.Importing as TS
import TreeSitter.Python

parseByteString :: B.ByteString -> IO (Maybe Module)
parseByteString = TS.parseByteString tree_sitter_python

main :: IO ()
main = do
  parseByteString "" >>= print
