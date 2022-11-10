{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import           TreeSitter.Parser
import           TreeSitter.Tree
import           TreeSitter.Query
import           TreeSitter.Haskell
import           TreeSitter.Node
import           Foreign.C.String
import           Foreign.Ptr                    ( nullPtr)
import           Foreign.Marshal.Alloc          ( malloc)
import           Foreign.Storable               ( peek
                                                , poke
                                                )

main :: IO ()
main = do
  parser <- ts_parser_new
  _      <- ts_parser_set_language parser tree_sitter_haskell
  let source = "module Test (main) where\nimport Lib\nf1 = undefined"

  (str, len)  <- newCStringLen source
  tree        <- ts_parser_parse_string parser nullPtr str len
  nodePointer <- malloc
  ts_tree_root_node_p tree nodePointer

  let source = "(function_body) @f"
  (str, len)  <- newCStringLen source
  errorOffset <- malloc
  errorType   <- malloc
  cursor      <- ts_query_cursor_new
  query       <- ts_query_new tree_sitter_haskell str len errorOffset errorType
  ts_query_cursor_exec_p cursor query nodePointer
  matchPointer <- malloc

  success      <- ts_query_cursor_next_match cursor matchPointer
  print $ "success: " ++ show success
  match   <- (peek matchPointer)
  capture <- peek $ matchCaptures match
  print capture

  -- print the match
  ts_node <- malloc
  poke ts_node (captureTSNode capture)
  ts_node_string_p ts_node >>= peekCString >>= print

  success <- ts_query_cursor_next_match cursor matchPointer
  print $ "success: " ++ show success

