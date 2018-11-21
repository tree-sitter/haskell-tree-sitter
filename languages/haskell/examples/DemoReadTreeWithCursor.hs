{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Monad

import           TreeSitter.Cursor

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
import qualified Data.Tree as T
import qualified Data.Tree.Zipper as Z


main :: IO ()
main = do
  parser <- ts_parser_new
  ts_parser_set_language parser tree_sitter_haskell

  let source =
        "module Test (main) where\nimport Lib\nf1 = undefined\nf2 = undefined"
  (str, len) <- newCStringLen source

  tree       <- ts_parser_parse_string parser nullPtr str len

  fgnPtr     <- mallocForeignPtr :: IO (ForeignPtr Cursor)
  addForeignPtrFinalizer funptr_ts_ptr_free fgnPtr

  withForeignPtr fgnPtr $ \cur -> do
    ts_ptr_init tree cur

    tree <- traverseTreeSitter cur
    putStrLn $ T.drawTree tree

    poss <- traverseTreeSitterPositions cur
    putStrLn $ show $ reverse poss
    