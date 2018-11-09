{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Monad
import           System.IO.Unsafe

import           TreeSitter.Ptr

import           TreeSitter.Parser
import           TreeSitter.Tree
import           TreeSitter.Language
import           TreeSitter.Haskell
import           TreeSitter.Node

import           Foreign.ForeignPtr
import           Foreign.C
import           Foreign.C.String
import           Foreign.C.Types
import           Foreign.Ptr                    ( Ptr(..)
                                                , nullPtr
                                                )

main :: IO ()
main = do
  parser <- ts_parser_new
  ts_parser_set_language parser tree_sitter_haskell

  let source =
        "module Test (main) where\nimport Lib\nf1 = undefined\nf2 = undefined"
  (str, len) <- newCStringLen source

  tree       <- ts_parser_parse_string parser nullPtr str len

  fgnPtr     <- mallocForeignPtr :: IO (ForeignPtr TreeSitter_Ptr)
  addForeignPtrFinalizer funptr_ts_ptr_free fgnPtr

  withForeignPtr fgnPtr $ \ptr -> do
    ts_ptr_init tree ptr
    -- when (status /= 0) $ fail "error in init"
    ts_ptr_goto_first_child ptr
    ts_ptr_goto_next_sibling ptr
    ts_ptr_goto_next_sibling ptr

    nodeType <- ts_ptr_current_type ptr
    theType <- peekCString nodeType
    print theType
