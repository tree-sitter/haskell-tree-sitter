{-# LANGUAGE DeriveGeneric #-}

module TreeSitter.Query
  ( TSQuery
  , TSQueryCursor
  , TSQueryError(..)
  , captureTSNode
  , captureIndex
  , matchId
  , matchPatternindex
  , matchCaptureCount
  , matchCaptures
  , ts_query_new
  , ts_query_cursor_exec_p
  , ts_query_cursor_new
  , ts_query_cursor_next_match
  )
where

import           Foreign
import           Foreign.C                      ( CString )
import           GHC.Generics
import           TreeSitter.Language
import           TreeSitter.Node
import           TreeSitter.Struct

-- | A tree-sitter query.
--
--   This type is uninhabited and used only for type safety within 'Ptr' values.
data TSQuery

-- | A tree-sitter query cursor.
--
--   This type is uninhabited and used only for type safety within 'Ptr' values.
data TSQueryCursor

data TSQueryError
  = TSQueryErrorNone
  | TSQueryErrorSyntax
  | TSQueryErrorNodeType
  | TSQueryErrorField
  | TSQueryErrorCapture
  deriving (Show, Enum, Eq)

data TSQueryCapture = TSQueryCapture
  { captureTSNode :: !TSNode
  , captureIndex :: !Word32
  } deriving (Show, Eq, Generic)

data TSQueryMatch = TSQueryMatch
  { matchId :: !Word32
  , matchPatternindex :: !Word16
  , matchCaptureCount :: !Word16
  , matchCaptures :: Ptr TSQueryCapture
  } deriving (Show, Eq, Generic)

instance Storable TSQueryError where
  alignment _ = alignment (0 :: Int8)
  {-# INLINE alignment #-}
  sizeOf _ = 1
  {-# INLINE sizeOf #-}
  peek = evalStruct $ toEnum <$> peekStruct
  {-# INLINE peek #-}
  poke ptr e = evalStruct (pokeStruct $ fromEnum e) ptr 
  {-# INLINE poke #-}

instance Storable TSQueryCapture where
  alignment _ = alignment (0 :: Int32)
  {-# INLINE alignment #-}
  sizeOf _ = 10
  {-# INLINE sizeOf #-}
  peek = evalStruct $ TSQueryCapture <$> peekStruct <*> peekStruct
  {-# INLINE peek #-}
  poke ptr (TSQueryCapture a b) = flip evalStruct ptr $ do
    pokeStruct a
    pokeStruct b
  {-# INLINE poke #-}

instance Storable TSQueryMatch where
  alignment _ = alignment (0 :: Int32)
  {-# INLINE alignment #-}
  sizeOf _ = 12
  {-# INLINE sizeOf #-}
  peek =
    evalStruct
      $   TSQueryMatch
      <$> peekStruct
      <*> peekStruct
      <*> peekStruct
      <*> peekStruct
  {-# INLINE peek #-}
  poke ptr (TSQueryMatch a b c d) = flip evalStruct ptr $ do
    pokeStruct a
    pokeStruct b
    pokeStruct c
    pokeStruct d
  {-# INLINE poke #-}


foreign import ccall safe "ts_query_new" ts_query_new :: Ptr Language -> CString -> Int -> Ptr Word32 -> Ptr TSQueryError -> IO (Ptr TSQuery)
foreign import ccall safe "ts_query_cursor_new" ts_query_cursor_new :: IO (Ptr TSQueryCursor)
foreign import ccall safe "src/bridge.c ts_query_cursor_exec_p" ts_query_cursor_exec_p :: Ptr TSQueryCursor -> Ptr TSQuery -> Ptr Node -> IO ()
foreign import ccall safe "ts_query_cursor_next_match" ts_query_cursor_next_match :: Ptr TSQueryCursor -> Ptr TSQueryMatch -> IO Bool

