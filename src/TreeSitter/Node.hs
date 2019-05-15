{-# LANGUAGE DeriveGeneric, GeneralizedNewtypeDeriving, InterruptibleFFI, RankNTypes, ScopedTypeVariables #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}
module TreeSitter.Node
( Node(..)
, TSPoint(..)
, TSNode(..)
, FieldId(..)
, ts_node_copy_child_nodes
) where

import Foreign
import Foreign.C
import GHC.Generics

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

newtype FieldId = FieldId { getFieldId :: Word16 }
  deriving (Eq, Ord, Show, Storable)


-- | 'Struct' is a strict 'Monad' with automatic alignment & advancing, & inferred type.
newtype Struct a = Struct { runStruct :: forall b . Ptr b -> IO (a, Ptr a) }

evalStruct :: Struct a -> Ptr b -> IO a
evalStruct s p = fmap fst $! runStruct s p
{-# INLINE evalStruct #-}

peekStruct :: forall a . Storable a => Struct a
peekStruct = Struct (\ p -> do
  let aligned = alignPtr (castPtr p) (alignment (undefined :: a))
  a <- peek aligned
  pure (a, aligned `plusPtr` sizeOf a))
{-# INLINE peekStruct #-}

pokeStruct :: Storable a => a -> Struct ()
pokeStruct a = Struct (\ p -> do
  let aligned = alignPtr (castPtr p) (alignment a)
  poke aligned a
  pure ((), castPtr aligned `plusPtr` sizeOf a))
{-# INLINE pokeStruct #-}


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

instance Functor Struct where
  fmap f a = Struct (\ p -> do
    (a', p') <- runStruct a p
    let fa = f a'
    fa `seq` p' `seq` pure (fa, castPtr p'))
  {-# INLINE fmap #-}

instance Applicative Struct where
  pure a = Struct (\ p -> pure (a, castPtr p))
  {-# INLINE pure #-}

  f <*> a = Struct (\ p -> do
    (f', p')  <- runStruct f          p
    (a', p'') <- p' `seq` runStruct a (castPtr p')
    let fa = f' a'
    fa `seq` p'' `seq` pure (fa, castPtr p''))
  {-# INLINE (<*>) #-}

instance Monad Struct where
  return = pure
  {-# INLINE return #-}

  a >>= f = Struct (\ p -> do
    (a', p')   <- runStruct a               p
    (fa', p'') <- p' `seq` runStruct (f a') (castPtr p')
    fa' `seq` p'' `seq` pure (fa', p''))
  {-# INLINE (>>=) #-}


foreign import ccall interruptible "src/bridge.c ts_node_copy_child_nodes" ts_node_copy_child_nodes :: Ptr TSNode -> Ptr Node -> IO ()
