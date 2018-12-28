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

import qualified Data.ByteString.Unsafe as BSU
import qualified Data.Text.Encoding as DTE
import qualified Data.Text as T


main :: IO ()
main = 

  BSU.unsafeUseAsCStringLen "module Test (f1) where\nimport Lib\nf1 = f2 42\nf2 n = n + 1" $ \ (str, len) -> do
    tree <- hts_parse_with_language tree_sitter_haskell str (fromIntegral len)

    ptrCursor <- mallocForeignPtr :: IO (ForeignPtr Cursor)
    addForeignPtrFinalizer funptr_ts_cursor_free ptrCursor

    withForeignPtr ptrCursor $ \cur -> do

      ts_cursor_init tree cur
      z <- tsTransformZipper cur
      putStrLn $ T.drawTree $ Z.toTree z

      -- 1st edit

      BSU.unsafeUseAsCStringLen "module Test (f) where\nimport Lib\nf1 = f2 42\nf2 n = n + 1" $ \ (str', len) -> do
        tree' <- ts_edit_tree_and_parse tree str' (fromIntegral len) 14 15 14 14 14 0 15 0 14

        ts_cursor_reset_root tree' cur
        z <- tsTransformZipper cur
        putStrLn $ T.drawTree $ Z.toTree z

        ts_cursor_reset_root tree' cur
        spanInfos <- tsTransformSpanInfos cur
        print (reverse spanInfos)

    -- TODO migrate to unsafeUseAsCStringLen
    
    -- 2nd edit

    -- str'' <- newCString
    --   "module Test f1) where\nimport Lib\nf1 = f2 42\nf2 n =  + 1"
    -- tree'' <- ts_edit_tree_and_parse tree' str'' 51 52 51 3 8 3 9 3 8

    -- ts_cursor_reset_root tree'' cur
    -- z <- tsTransformZipper cur
    -- putStrLn $ T.drawTree $ Z.toTree z

    -- ts_cursor_reset_root tree'' cur
    -- spanInfos <- tsTransformSpanInfos cur
    -- print (reverse spanInfos)

    -- -- 3rd edit

    -- str''' <- newCString
    --   "module Test f1) where\nimport Lib\nf1 = f2 42\nf2 n = abc + 1"
    -- tree''' <- ts_edit_tree_and_parse tree'' str''' 51 51 54 3 8 3 8 3 11

    -- ts_cursor_reset_root tree''' cur
    -- z <- tsTransformZipper cur
    -- putStrLn $ T.drawTree $ Z.toTree z

    -- ts_cursor_reset_root tree''' cur
    -- spanInfos <- tsTransformSpanInfos cur
    -- print (reverse spanInfos)
