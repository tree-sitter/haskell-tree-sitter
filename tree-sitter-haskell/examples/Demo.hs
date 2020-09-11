{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Main (main) where

import qualified Data.Tree as Tree
import           TreeSitter.Parser
import           TreeSitter.Tree
import           TreeSitter.Query
-- import           TreeSitter.Language
import           TreeSitter.Haskell
import           TreeSitter.Node
import           Foreign.C.String
import           Foreign.Ptr                    ( Ptr
                                                , nullPtr
                                                )
import           Foreign.Marshal.Alloc          ( malloc)
import           Foreign.Marshal.Array          ( mallocArray )
import           Foreign.Storable               ( peek
                                                , peekElemOff
                                                , poke
                                                )
import           Control.Monad

main :: IO ()
main = do
  parser <- ts_parser_new
  _      <- ts_parser_set_language parser tree_sitter_haskell
  let source = "module Test (main) where\nimport Lib\nf1 = undefined"
                -- "module Test (main) where\nimport Lib\nf1 = undefined\nf2 = undefined"

  (str, len)  <- newCStringLen source
  tree        <- ts_parser_parse_string parser nullPtr str len
  nodePointer <- malloc
  ts_tree_root_node_p tree nodePointer
  node <- peek nodePointer
  print =<< nodeString node
  -- ast <- toAst node
  -- print ast

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
  capture <- peek $ captures match
  print capture

  ts_node <- malloc
  poke ts_node (captureTSNode capture)
  ts_node_string_p ts_node >>= peekCString >>= print

  success <- ts_query_cursor_next_match cursor matchPointer
  print $ "success: " ++ show success

type Ast = Tree.Tree Node'
data Node' = Node' {type_::String, text :: String} deriving Show

toAst :: Node -> IO Ast
toAst Node {..} = do
  -- Node {..} <- peek n
  theType <- peekCString nodeType
  let childCount = fromIntegral nodeChildCount
  children <- mallocArray childCount
  tsNode   <- malloc
  poke                     tsNode nodeTSNode
  ts_node_copy_child_nodes tsNode children
  text      <- ts_node_string_p tsNode >>= peekCString
  children' <- forM [0 .. childCount - 1] $ \n -> do
    child <- peekElemOff children n
    toAst child

  return $ Tree.Node (Node' theType text) children'


printChildren :: Ptr Node -> Int -> IO ()
printChildren children count = forM_
  [0 .. count - 1]
  ( \n -> do
    child <- peekElemOff children n
    printNode child
  )

printNode :: Node -> IO ()
printNode n@Node {..} = do
  theType <- peekCString nodeType
  let TSPoint {..} = nodeStartPoint n
      start        = "(" ++ show pointRow ++ "," ++ show pointColumn ++ ")"
  let TSPoint {..} = nodeEndPoint
      end          = "(" ++ show pointRow ++ "," ++ show pointColumn ++ ")"
  print $ theType ++ start ++ "-" ++ end
