module TreeSitter.Cursor where

import Data.Int
import Data.Word
import Foreign.C
import Foreign.Ptr
import TreeSitter.Node

-- | A cursor for traversing a tree.
--
--   Note that we do not define 'Eq', 'Ord', or 'Storable' instances, as the underlying @TSTreeCursor@ type is not usefully copyable.
data Cursor = Cursor
  deriving (Show)

-- | THe size of a 'Cursor' in bytes. The tests verify that this value is the same as @sizeof(TSTreeCursor)@.
sizeOfCursor :: Int
sizeOfCursor = 72

sizeOfCursor :: Int
sizeOfCursor = 72

foreign import ccall unsafe "src/bridge.c ts_tree_cursor_new_p" ts_tree_cursor_new_p :: Ptr TSNode -> Ptr Cursor -> IO ()
foreign import ccall unsafe "ts_tree_cursor_delete" ts_tree_cursor_delete :: Ptr Cursor -> IO ()
foreign import ccall unsafe "src/bridge.c ts_tree_cursor_reset_p" ts_tree_cursor_reset_p :: Ptr Cursor -> Ptr TSNode -> IO ()

foreign import ccall unsafe "src/bridge.c ts_tree_cursor_current_node_p" ts_tree_cursor_current_node_p :: Ptr Cursor -> Ptr TSNode -> IO ()
foreign import ccall unsafe "ts_tree_cursor_current_field_name" ts_tree_cursor_current_field_name :: Ptr Cursor -> IO CString
foreign import ccall unsafe "ts_tree_cursor_current_field_id" ts_tree_cursor_current_field_id :: Ptr Cursor -> IO FieldId

foreign import ccall unsafe "ts_tree_cursor_goto_parent" ts_tree_cursor_goto_parent :: Ptr Cursor -> IO Bool
foreign import ccall unsafe "ts_tree_cursor_goto_next_sibling" ts_tree_cursor_goto_next_sibling :: Ptr Cursor -> IO Bool
foreign import ccall unsafe "ts_tree_cursor_goto_first_child" ts_tree_cursor_goto_first_child :: Ptr Cursor -> IO Bool
foreign import ccall unsafe "ts_tree_cursor_goto_first_child_for_byte" ts_tree_cursor_goto_first_child_for_byte :: Ptr Cursor -> Word32 -> IO Int64
