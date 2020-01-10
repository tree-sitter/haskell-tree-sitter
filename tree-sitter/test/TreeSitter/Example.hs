{-# LANGUAGE TemplateHaskell #-}
module TreeSitter.Example (tests) where

import Control.Monad.IO.Class
import Foreign
import Foreign.C.Types
import Hedgehog
import TreeSitter.Cursor
import TreeSitter.Node
import TreeSitter.Parser

tests :: IO Bool
tests = checkSequential $$(discover)

prop_TSNode_sizeOf = property $
  sizeOf (undefined :: TSNode) === fromIntegral sizeof_tsnode

prop_TSNode_roundtrips = property $ do
  peeked <- liftIO (with (TSNode 1 (TSPoint 2 3) 4 nullPtr nullPtr) peek)
  peeked ===              TSNode 1 (TSPoint 2 3) 4 nullPtr nullPtr

prop_TSPoint_sizeOf = property $
  sizeOf (undefined :: TSPoint) === fromIntegral sizeof_tspoint

prop_TSPoint_roundtrips = property $ do
  peeked <- liftIO (with (TSPoint 1 2) peek)
  peeked ===              TSPoint 1 2

prop_Node_sizeOf = property $
  sizeOf (undefined :: Node) === fromIntegral sizeof_node

prop_Node_roundtrips = property $ do
  peeked <- liftIO (with (Node (TSNode 1 (TSPoint 2 3) 4 nullPtr nullPtr) nullPtr 1 (TSPoint 4 5) 7 8 nullPtr 9 10) peek)
  peeked ===              Node (TSNode 1 (TSPoint 2 3) 4 nullPtr nullPtr) nullPtr 1 (TSPoint 4 5) 7 8 nullPtr 9 10

prop_TSTreeCursor_sizeOf = property $
  sizeOfCursor === fromIntegral sizeof_tstreecursor

prop_Parser_timeout = property $ do
  parser <- liftIO ts_parser_new
  timeout <- liftIO (ts_parser_timeout_micros parser)
  timeout === 0
  liftIO (ts_parser_set_timeout_micros parser 1000)
  timeout <- liftIO (ts_parser_timeout_micros parser)
  timeout === 1000
  liftIO (ts_parser_delete parser)

foreign import ccall unsafe "src/bridge.c sizeof_tsnode" sizeof_tsnode :: CSize
foreign import ccall unsafe "src/bridge.c sizeof_tspoint" sizeof_tspoint :: CSize
foreign import ccall unsafe "src/bridge.c sizeof_node" sizeof_node :: CSize
foreign import ccall unsafe "src/bridge.c sizeof_tstreecursor" sizeof_tstreecursor :: CSize
