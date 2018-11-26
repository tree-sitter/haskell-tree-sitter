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
import           Foreign.Storable               ( poke
                                                )

import qualified Data.Tree as T
import qualified Data.Tree.Zipper as Z


main :: IO ()
main = do
  parser <- ts_parser_new
  ts_parser_set_language parser tree_sitter_haskell
  (str, len) <- newCStringLen "module Test (f1) where\nimport Lib\nf1 = f2 42\nf2 n = n + 1"

  tree       <- ts_parser_parse_string parser nullPtr str len
  ptrCursor  <- mallocForeignPtr :: IO (ForeignPtr Cursor)
  addForeignPtrFinalizer funptr_ts_cursor_free ptrCursor

  ptrTsInputEdit  <- mallocForeignPtr :: IO (ForeignPtr TSInputEdit)

  withForeignPtr ptrCursor $ \cur -> do

    ts_cursor_init tree cur
    z <- tsTransformZipper cur
    putStrLn $ T.drawTree $ Z.toTree z

    withForeignPtr ptrTsInputEdit $ \edit -> do
      poke edit TSInputEdit {
          startByte = (fromInteger 0)
          , oldEndByte = (fromInteger 0)
          , newEndByte = (fromInteger 0)
          , startPoint = TSPoint {pointRow = (fromInteger 0), pointColumn = (fromInteger 0)}
          , oldEndPoint = TSPoint {pointRow = (fromInteger 0), pointColumn = (fromInteger 0)}
          , newEndPoint = TSPoint {pointRow = (fromInteger 0), pointColumn = (fromInteger 0)}
          }
      -- poke edit TSInputEdit {
      --     startByte = (fromInteger 12)
      --     , oldEndByte = (fromInteger 13)
      --     , newEndByte = (fromInteger 2)
      --     , startPoint = TSPoint {pointRow = (fromInteger 0), pointColumn = (fromInteger 12)}
      --     , oldEndPoint = TSPoint {pointRow = (fromInteger 0), pointColumn = (fromInteger 13)}
      --     , newEndPoint = TSPoint {pointRow = (fromInteger 0), pointColumn = (fromInteger 12)}
      --     }
      ts_tree_edit tree edit
      (str, len) <- newCStringLen "module Test f1) where\nimport Lib\nf1 = f2 42\nf2 n = n + 1"
      tree <- ts_parser_parse_string parser nullPtr str len

      ts_cursor_init tree cur
      z <- tsTransformZipper cur
      putStrLn $ T.drawTree $ Z.toTree z

