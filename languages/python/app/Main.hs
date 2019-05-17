{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Control.Exception as Exc
import qualified Data.ByteString as B
import qualified TreeSitter.Importing as TS
import TreeSitter.Parser
import TreeSitter.Python

parseByteString :: B.ByteString -> IO (Maybe Module)
parseByteString bytestring = Exc.bracket
  ts_parser_new
  ts_parser_delete
  $ \ parser -> do
    ts_parser_set_language parser tree_sitter_python
    TS.parseByteString parser bytestring

main :: IO ()
main = do
  parseByteString "" >>= print
