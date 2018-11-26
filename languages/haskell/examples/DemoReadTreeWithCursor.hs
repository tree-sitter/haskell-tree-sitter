{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Monad

import           TreeSitter.CursorApi.Cursor

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
  (str, len) <- newCStringLen "module Test (f1) where\nimport Lib\nf1 = f2 42\nf2 n = n + 1"

  tree       <- ts_parser_parse_string parser nullPtr str len
  fgnPtr     <- mallocForeignPtr :: IO (ForeignPtr Cursor)
  addForeignPtrFinalizer funptr_ts_cursor_free fgnPtr

  withForeignPtr fgnPtr $ \cur -> do
    ts_cursor_init tree cur

    z <- tsTransformZipper cur
    putStrLn $ T.drawTree $ Z.toTree z

    spanInfos <- tsTransformSpanInfos cur
    print (reverse spanInfos)
    