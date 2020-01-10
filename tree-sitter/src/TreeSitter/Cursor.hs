module TreeSitter.Cursor
( Cursor
, withCursor
, sizeOfCursor
, ts_tree_cursor_new_p
, ts_tree_cursor_delete
, ts_tree_cursor_reset_p
, ts_tree_cursor_current_node_p
, ts_tree_cursor_current_field_name
, ts_tree_cursor_current_field_id
, ts_tree_cursor_goto_parent
, ts_tree_cursor_goto_next_sibling
, ts_tree_cursor_goto_first_child
, ts_tree_cursor_goto_first_child_for_byte
, ts_tree_cursor_copy_child_nodes
) where

import Control.Exception as Exc
import Data.Int
import Data.Word
import Foreign.C
import Foreign.Marshal.Alloc
import Foreign.Ptr
import TreeSitter.Node

-- | A cursor for traversing a tree.
--
--   This type is uninhabited and used only for type safety within 'Ptr' values.
data Cursor

withCursor :: Ptr TSNode -> (Ptr Cursor -> IO a) -> IO a
withCursor rootPtr action = allocaBytes sizeOfCursor $ \ cursor -> Exc.bracket
  (cursor <$ ts_tree_cursor_new_p rootPtr cursor)
  ts_tree_cursor_delete
  action

-- | THe size of a 'Cursor' in bytes. The tests verify that this value is the same as @sizeof(TSTreeCursor)@.
sizeOfCursor :: Int
sizeOfCursor = 24

foreign import ccall unsafe "src/bridge.c ts_tree_cursor_new_p" ts_tree_cursor_new_p :: Ptr TSNode -> Ptr Cursor -> IO ()
foreign import ccall unsafe "ts_tree_cursor_delete" ts_tree_cursor_delete :: Ptr Cursor -> IO ()
foreign import ccall unsafe "src/bridge.c ts_tree_cursor_reset_p" ts_tree_cursor_reset_p :: Ptr Cursor -> Ptr TSNode -> IO ()

foreign import ccall unsafe "src/bridge.c ts_tree_cursor_current_node_p" ts_tree_cursor_current_node_p :: Ptr Cursor -> Ptr Node -> IO Bool
foreign import ccall unsafe "ts_tree_cursor_current_field_name" ts_tree_cursor_current_field_name :: Ptr Cursor -> IO CString
foreign import ccall unsafe "ts_tree_cursor_current_field_id" ts_tree_cursor_current_field_id :: Ptr Cursor -> IO FieldId

foreign import ccall unsafe "ts_tree_cursor_goto_parent" ts_tree_cursor_goto_parent :: Ptr Cursor -> IO Bool
foreign import ccall unsafe "ts_tree_cursor_goto_next_sibling" ts_tree_cursor_goto_next_sibling :: Ptr Cursor -> IO Bool
foreign import ccall unsafe "ts_tree_cursor_goto_first_child" ts_tree_cursor_goto_first_child :: Ptr Cursor -> IO Bool
foreign import ccall unsafe "ts_tree_cursor_goto_first_child_for_byte" ts_tree_cursor_goto_first_child_for_byte :: Ptr Cursor -> Word32 -> IO Int64

foreign import ccall unsafe "src/bridge.c ts_tree_cursor_copy_child_nodes" ts_tree_cursor_copy_child_nodes :: Ptr Cursor -> Ptr Node -> IO Word32
