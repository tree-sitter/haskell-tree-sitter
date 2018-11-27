{-# LANGUAGE DeriveGeneric, InterruptibleFFI #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}
module TreeSitter.TsInputEdit
  ( TSInputEdit(..)
  , ts_tree_edit
  , ts_edit_node_in_range
  )
where

import           Foreign
import           GHC.Generics

import           TreeSitter.Tree
import           TreeSitter.Struct
import           TreeSitter.TsPoint


data TSInputEdit = TSInputEdit {
    startByte :: !Word32
    , oldEndByte :: !Word32
    , newEndByte :: !Word32
    , startPoint :: !TSPoint
    , oldEndPoint :: !TSPoint
    , newEndPoint :: !TSPoint
  }
  deriving (Show, Eq, Generic)

instance Storable TSInputEdit where
  alignment _ = alignment (0 :: Int32)
  sizeOf _ = 8
  peek = evalStruct $ TSInputEdit <$> peekStruct
                                  <*> peekStruct
                                  <*> peekStruct
                                  <*> peekStruct
                                  <*> peekStruct
                                  <*> peekStruct
  poke ptr (TSInputEdit sb oeb neb sp oep nep) = flip evalStruct ptr $ do
    pokeStruct sb
    pokeStruct oeb
    pokeStruct neb
    pokeStruct sp
    pokeStruct oep
    pokeStruct nep


foreign import ccall ts_tree_edit :: Ptr Tree -> Ptr TSInputEdit -> IO ()
foreign import ccall ts_edit_node_in_range :: Ptr Tree -> Word32 -> Word32 -> Ptr TSInputEdit -> IO ()
