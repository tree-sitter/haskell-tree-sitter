{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Main where

import           TreeSitter.Parser
import           TreeSitter.Tree
import           TreeSitter.Language
import           TreeSitter.Haskell
import           TreeSitter.Node
import           Foreign.C.String
import           Foreign.C.Types
import           Foreign.Ptr                    ( Ptr(..)
                                                , nullPtr
                                                , plusPtr
                                                )
import           Foreign.Marshal.Alloc          ( malloc
                                                , mallocBytes
                                                )
import           Foreign.Marshal.Array          ( mallocArray )
import           Foreign.Storable               ( peek
                                                , peekElemOff
                                                , poke
                                                )
import           Foreign.Marshal.Utils          ( new )
import           Control.Monad


main :: IO ()
main = do
  parser <- ts_parser_new
  ts_parser_set_language parser tree_sitter_haskell

  let source =
        "module Test (main) where\nimport Lib\nf1 = undefined\nf2 = undefined"

  (str, len) <- newCStringLen source
  tree       <- ts_parser_parse_string parser nullPtr str len

  n          <- malloc
  ts_tree_root_node_p tree n

  print "module (root) ------------"
  n@Node {..} <- peek n
  let childCount = fromIntegral nodeChildCount

  children <- mallocArray childCount
  tsNode   <- malloc
  poke tsNode nodeTSNode
  ts_node_copy_child_nodes tsNode children

  printChildren children childCount

  print "where ------------"
  n@Node {..} <- peekElemOff children 3
  let nextChildCount = fromIntegral nodeChildCount

  nextChildren <- mallocArray nextChildCount
  nextTsNode   <- malloc
  poke nextTsNode nodeTSNode
  ts_node_copy_child_nodes nextTsNode nextChildren

  printChildren nextChildren nextChildCount

  print "END"

printChildren :: Ptr Node -> Int -> IO ()
printChildren children count = forM_
  [0 .. count - 1]
  (\n -> do
    child <- peekElemOff children n
    printNode child
  )

printNode :: Node -> IO ()
printNode Node {..} = do
  theType <- peekCString nodeType
  let TSPoint {..} = nodeStartPoint
      start        = "(" ++ show pointRow ++ "," ++ show pointColumn ++ ")"
  let TSPoint {..} = nodeEndPoint
      end          = "(" ++ show pointRow ++ "," ++ show pointColumn ++ ")"
  print $ theType ++ start ++ "-" ++ end
