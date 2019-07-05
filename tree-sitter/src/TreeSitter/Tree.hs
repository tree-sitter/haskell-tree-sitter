module TreeSitter.Tree
( Tree
, withRootNode
, ts_tree_delete
, ts_tree_root_node_p
) where

import Foreign
import TreeSitter.Node

-- | This type is uninhabited and used only for type safety within 'Ptr' values.
data Tree

withRootNode :: Ptr Tree -> (Ptr Node -> IO a) -> IO a
withRootNode tree action = alloca $ \ ptr -> do
  ts_tree_root_node_p tree ptr
  action ptr

foreign import ccall safe "ts_tree_delete" ts_tree_delete :: Ptr Tree -> IO ()
foreign import ccall unsafe "src/bridge.c ts_tree_root_node_p" ts_tree_root_node_p :: Ptr Tree -> Ptr Node -> IO ()
