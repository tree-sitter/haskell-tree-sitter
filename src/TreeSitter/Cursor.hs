module TreeSitter.Cursor where

import Foreign.C
import Foreign.Ptr
import TreeSitter.Node

data Cursor = Cursor

sizeOfCursor :: Int
sizeOfCursor = 72

foreign import ccall unsafe "src/bridge.c ts_tree_cursor_new_p" ts_tree_cursor_new_p :: Ptr TSNode -> Ptr Cursor -> IO ()
foreign import ccall unsafe "ts_tree_cursor_delete" ts_tree_cursor_delete :: Ptr Cursor -> IO ()

foreign import ccall unsafe "src/bridge.c ts_tree_cursor_current_node_p" ts_tree_cursor_current_node_p :: Ptr Cursor -> Ptr TSNode -> IO ()
foreign import ccall unsafe "ts_tree_cursor_current_field_name" ts_tree_cursor_current_field_name :: Ptr Cursor -> IO CString
foreign import ccall unsafe "ts_tree_cursor_current_field_id" ts_tree_cursor_current_field_id :: Ptr Cursor -> IO FieldId

foreign import ccall unsafe "ts_tree_cursor_goto_parent" ts_tree_cursor_goto_parent :: Ptr Cursor -> IO Bool
foreign import ccall unsafe "ts_tree_cursor_goto_next_sibling" ts_tree_cursor_goto_next_sibling :: Ptr Cursor -> IO Bool
foreign import ccall unsafe "ts_tree_cursor_goto_first_child" ts_tree_cursor_goto_first_child :: Ptr Cursor -> IO Bool
