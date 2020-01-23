{-# LANGUAGE DeriveGeneric, GeneralizedNewtypeDeriving, RankNTypes, ScopedTypeVariables #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}
module TreeSitter.Node
( Node(..)
, nodeStartPoint
, nodeStartByte
, TSPoint(..)
, TSNode(..)
, FieldId(..)
, ts_node_copy_child_nodes
, ts_node_poke_p
) where

import Foreign
import Foreign.C
import GHC.Generics
import TreeSitter.Symbol (TSSymbol)

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

instance Functor Struct where
  fmap f a = Struct go where
    go p = do
      (a', p') <- runStruct a p
      let fa = f a'
      fa `seq` p' `seq` pure (fa, castPtr p')
    {-# INLINE go #-}
  {-# INLINE fmap #-}

instance Applicative Struct where
  pure a = Struct (\ p -> pure (a, castPtr p))
  {-# INLINE pure #-}

  f <*> a = Struct go where
    go p = do
      (f', p')  <- runStruct f          p
      (a', p'') <- p' `seq` runStruct a (castPtr p')
      let fa = f' a'
      fa `seq` p'' `seq` pure (fa, castPtr p'')
    {-# INLINE go #-}
  {-# INLINE (<*>) #-}

instance Monad Struct where
  return = pure
  {-# INLINE return #-}

  a >>= f = Struct go where
    go p = do
      (a', p')   <- runStruct a               p
      (fa', p'') <- p' `seq` runStruct (f a') (castPtr p')
      fa' `seq` p'' `seq` pure (fa', p'')
    {-# INLINE go #-}
  {-# INLINE (>>=) #-}


foreign import ccall unsafe "src/bridge.c ts_node_copy_child_nodes" ts_node_copy_child_nodes :: Ptr TSNode -> Ptr Node -> IO ()
-- NB: this leaves the field name as NULL.
foreign import ccall unsafe "src/bridge.c ts_node_poke_p" ts_node_poke_p :: Ptr TSNode -> Ptr Node -> IO ()
