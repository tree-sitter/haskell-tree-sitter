{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Monad

import           TreeSitter.CursorApi.Cursor

import           TreeSitter.Parser
import           TreeSitter.Tree
import           TreeSitter.Language
import           TreeSitter.Haskell
import           TreeSitter.Node
import           TreeSitter.TsInputEdit
import           TreeSitter.TsPoint

import           Foreign.ForeignPtr
import           Foreign.C
import           Foreign.C.String
import           Foreign.C.Types
import           Foreign.Ptr                    ( Ptr(..)
                                                , nullPtr
                                                )

import qualified Data.Tree                     as T
import qualified Data.Tree.Zipper              as Z


main :: IO ()
main = do
  parser <- ts_parser_new
  ts_parser_set_language parser tree_sitter_haskell
  (str, len) <- newCStringLen
    "module Test (f1) where\nimport Lib\nf1 = f2 42\nf2 n = n + 1"

  tree      <- ts_parser_parse_string parser nullPtr str len
  ptrCursor <- mallocForeignPtr :: IO (ForeignPtr Cursor)
  addForeignPtrFinalizer funptr_ts_cursor_free ptrCursor

  withForeignPtr ptrCursor $ \cur -> do

    ts_cursor_init tree cur
    z <- tsTransformZipper cur
    putStrLn $ T.drawTree $ Z.toTree z

    -- 1st edit

    str' <- newCString
      "module Test f1) where\nimport Lib\nf1 = f2 42\nf2 n = n + 1"
    tree' <- ts_edit_tree_and_parse parser tree str' 12 13 2 0 12 0 13 0 12

    ts_cursor_reset_root tree' cur
    z <- tsTransformZipper cur
    putStrLn $ T.drawTree $ Z.toTree z

    -- 2nd edit

    str'' <- newCString
      "module Test f1) where\nimport Lib\nf1 = f2 42\nf2 n =  + 1"
    tree'' <- ts_edit_tree_and_parse parser tree' str'' 51 52 51 3 8 3 9 3 8

    ts_cursor_reset_root tree'' cur
    z <- tsTransformZipper cur
    putStrLn $ T.drawTree $ Z.toTree z

    ts_cursor_reset_root tree'' cur
    spanInfos <- tsTransformSpanInfos cur
    print (reverse spanInfos)

    -- 3rd edit

    str''' <- newCString
      "module Test f1) where\nimport Lib\nf1 = f2 42\nf2 n = abc + 1"
    tree''' <- ts_edit_tree_and_parse parser tree'' str''' 51 51 54 3 8 3 8 3 11

    ts_cursor_reset_root tree''' cur
    z <- tsTransformZipper cur
    putStrLn $ T.drawTree $ Z.toTree z

    ts_cursor_reset_root tree''' cur
    spanInfos <- tsTransformSpanInfos cur
    print (reverse spanInfos)
