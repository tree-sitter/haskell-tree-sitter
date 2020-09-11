{-# LANGUAGE DeriveGeneric, GeneralizedNewtypeDeriving, RankNTypes, ScopedTypeVariables #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}
module TreeSitter.Node
( Node(..)
, nodeStartPoint
, nodeStartByte
, nodeString
, TSPoint(..)
, TSNode(..)
, FieldId(..)
, ts_node_copy_child_nodes
, ts_node_poke_p
, ts_node_string_p
) where

import Foreign
import Foreign.C
import GHC.Generics
import TreeSitter.Symbol (TSSymbol)
import TreeSitter.Struct

data Node = Node
  { nodeTSNode     :: !TSNode
  , nodeType       :: !CString
  , nodeSymbol     :: !TSSymbol
  , nodeEndPoint   :: !TSPoint
  , nodeEndByte    :: !Word32
  , nodeChildCount :: !Word32
  , nodeFieldName  :: !CString
  , nodeIsNamed    :: !CBool
  , nodeIsExtra    :: !CBool
  }
  deriving (Show, Eq, Generic)

nodeString :: Node -> IO String
nodeString node = do
  ts_node <- malloc
  poke ts_node $ nodeTSNode node
  ts_node_string_p ts_node >>= peekCString

nodeStartPoint :: Node -> TSPoint
nodeStartPoint node = let TSNode _ p _ _ _ = nodeTSNode node in p

nodeStartByte :: Node -> Word32
nodeStartByte node = let TSNode b _ _ _ _ = nodeTSNode node in b

data TSPoint = TSPoint { pointRow :: !Word32, pointColumn :: !Word32 }
  deriving (Show, Eq, Generic)

data TSNode = TSNode !Word32 !TSPoint !Word32 !(Ptr ()) !(Ptr ())
  deriving (Show, Eq, Generic)

newtype FieldId = FieldId { getFieldId :: Word16 }
  deriving (Eq, Ord, Show, Storable)

instance Storable Node where
  alignment _ = alignment (undefined :: TSNode)
  {-# INLINE alignment #-}
  sizeOf _ = 80
  {-# INLINE sizeOf #-}
  peek = evalStruct $ Node <$> peekStruct
                           <*> peekStruct
                           <*> peekStruct
                           <*> peekStruct
                           <*> peekStruct
                           <*> peekStruct
                           <*> peekStruct
                           <*> peekStruct
                           <*> peekStruct
  {-# INLINE peek #-}
  poke ptr (Node n t s ep eb c fn nn ne) = flip evalStruct ptr $ do
    pokeStruct n
    pokeStruct t
    pokeStruct s
    pokeStruct ep
    pokeStruct eb
    pokeStruct c
    pokeStruct fn
    pokeStruct nn
    pokeStruct ne
  {-# INLINE poke #-}

instance Storable TSPoint where
  alignment _ = alignment (0 :: Int32)
  {-# INLINE alignment #-}
  sizeOf _ = 8
  {-# INLINE sizeOf #-}
  peek = evalStruct $ TSPoint <$> peekStruct
                              <*> peekStruct
  {-# INLINE peek #-}
  poke ptr (TSPoint r c) = flip evalStruct ptr $ do
    pokeStruct r
    pokeStruct c
  {-# INLINE poke #-}

instance Storable TSNode where
  alignment _ = alignment (nullPtr :: Ptr ())
  {-# INLINE alignment #-}
  sizeOf _ = 32
  {-# INLINE sizeOf #-}
  peek = evalStruct $ TSNode <$> peekStruct
                             <*> peekStruct
                             <*> peekStruct
                             <*> peekStruct
                             <*> peekStruct
  {-# INLINE peek #-}
  poke ptr (TSNode sb sp o4 p1 p2) = flip evalStruct ptr $ do
    pokeStruct sb
    pokeStruct sp
    pokeStruct o4
    pokeStruct p1
    pokeStruct p2
  {-# INLINE poke #-}

foreign import ccall unsafe "src/bridge.c ts_node_copy_child_nodes" ts_node_copy_child_nodes :: Ptr TSNode -> Ptr Node -> IO ()
-- NB: this leaves the field name as NULL.
foreign import ccall unsafe "src/bridge.c ts_node_poke_p" ts_node_poke_p :: Ptr TSNode -> Ptr Node -> IO ()
foreign import ccall unsafe "src/bridge.c ts_node_string_p" ts_node_string_p :: Ptr TSNode ->  IO CString
