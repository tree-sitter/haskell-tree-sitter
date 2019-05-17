module TreeSitter.Tree where

import Foreign
import TreeSitter.Node

data Tree = Tree
  deriving (Show)

foreign import ccall safe "ts_tree_delete" ts_tree_delete :: Ptr Tree -> IO ()
foreign import ccall unsafe "src/bridge.c ts_tree_root_node_p" ts_tree_root_node_p :: Ptr Tree -> Ptr Node -> IO ()
