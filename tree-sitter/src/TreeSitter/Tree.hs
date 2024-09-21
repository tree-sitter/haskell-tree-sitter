{-# LANGUAGE DeriveGeneric #-}
module TreeSitter.Tree
( Tree
, withRootNode
, ts_tree_edit
, ts_tree_delete
, ts_tree_root_node_p
) where

import Foreign
import TreeSitter.Node
import GHC.Generics

-- | This type is uninhabited and used only for type safety within 'Ptr' values.
data Tree

withRootNode :: Ptr Tree -> (Ptr Node -> IO a) -> IO a
withRootNode tree action = alloca $ \ ptr -> do
  ts_tree_root_node_p tree ptr
  action ptr

data TSInputEdit = TSInputEdit
  { start_byte :: !Word32
  , old_end_byte :: !Word32
  , new_end_byte :: !Word32
  , start_point :: !TSPoint
  , old_end_point :: !TSPoint
  , new_end_point :: !TSPoint
  }
  deriving (Show, Eq, Generic)

instance Storable TSInputEdit where
  alignment _ = alignment (0 :: Int32)
  sizeOf _ = 36
  peek = evalStruct $
    TSInputEdit <$> peekStruct
                <*> peekStruct
                <*> peekStruct
                <*> peekStruct
                <*> peekStruct
                <*> peekStruct
  poke ptr (TSInputEdit sb oldEb newEb sp oldEp newEp) =
    flip evalStruct ptr $ do
      pokeStruct sb
      pokeStruct oldEb
      pokeStruct newEb
      pokeStruct sp
      pokeStruct oldEp
      pokeStruct newEp

foreign import ccall safe "ts_tree_edit" ts_tree_edit :: Ptr Tree -> Ptr TSInputEdit -> IO ()
foreign import ccall safe "ts_tree_delete" ts_tree_delete :: Ptr Tree -> IO ()
foreign import ccall unsafe "src/bridge.c ts_tree_root_node_p" ts_tree_root_node_p :: Ptr Tree -> Ptr Node -> IO ()
