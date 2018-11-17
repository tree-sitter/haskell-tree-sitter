{-# LANGUAGE DeriveGeneric, DeriveAnyClass, InterruptibleFFI, RankNTypes, ScopedTypeVariables #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}
module TreeSitter.Node
( Node(..)
, TSPoint(..)
, TSNode(..)
, ts_node_copy_child_nodes
) where

import Foreign
import Foreign.C
import GHC.Generics

import TreeSitter.Struct


data Node = Node
  { nodeTSNode :: !TSNode
  , nodeType :: !CString
  , nodeSymbol :: !Word16
  , nodeStartPoint :: !TSPoint
  , nodeEndPoint :: !TSPoint
  , nodeStartByte :: !Word32
  , nodeEndByte :: !Word32
  , nodeChildCount :: !Word32
  }
  deriving (Show, Eq, Generic)

data TSPoint = TSPoint { pointRow :: !Word32, pointColumn :: !Word32 }
  deriving (Show, Eq, Generic)

data TSNode = TSNode !Word32 !Word32 !Word32 !Word32 !(Ptr ()) !(Ptr ())
  deriving (Show, Eq, Generic)


instance Storable Node where
  alignment _ = alignment (TSNode 0 0 0 0 nullPtr nullPtr :: TSNode)
  sizeOf _ = 72
  peek = evalStruct $ Node <$> peekStruct
                           <*> peekStruct
                           <*> peekStruct
                           <*> peekStruct
                           <*> peekStruct
                           <*> peekStruct
                           <*> peekStruct
                           <*> peekStruct
  poke ptr (Node n t s sp ep sb eb c) = flip evalStruct ptr $ do
    pokeStruct n
    pokeStruct t
    pokeStruct s
    pokeStruct sp
    pokeStruct ep
    pokeStruct sb
    pokeStruct eb
    pokeStruct c

instance Storable TSPoint where
  alignment _ = alignment (0 :: Int32)
  sizeOf _ = 8
  peek = evalStruct $ TSPoint <$> peekStruct
                              <*> peekStruct
  poke ptr (TSPoint r c) = flip evalStruct ptr $ do
    pokeStruct r
    pokeStruct c

instance Storable TSNode where
  alignment _ = alignment (nullPtr :: Ptr ())
  sizeOf _ = 32
  peek = evalStruct $ TSNode <$> peekStruct
                             <*> peekStruct
                             <*> peekStruct
                             <*> peekStruct
                             <*> peekStruct
                             <*> peekStruct
  poke ptr (TSNode o1 o2 o3 o4 p1 p2) = flip evalStruct ptr $ do
    pokeStruct o1
    pokeStruct o2
    pokeStruct o3
    pokeStruct o4
    pokeStruct p1
    pokeStruct p2

foreign import ccall interruptible "src/bridge.c ts_node_copy_child_nodes" ts_node_copy_child_nodes :: Ptr TSNode -> Ptr Node -> IO ()
