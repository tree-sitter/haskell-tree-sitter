{-# LANGUAGE DeriveGeneric, GeneralizedNewtypeDeriving, RankNTypes,
  ScopedTypeVariables, TypeOperators, DeriveAnyClass #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}

module TreeSitter.Query
  ( TSQuery
  , TSQueryCursor
  , pattern_index
  , captures
  , captureTSNode
  , ts_query_new
  , ts_query_cursor_exec_p
  , ts_query_cursor_new
  , ts_query_cursor_next_match
  ) where

import Foreign
import Foreign.C (CString)
import GHC.Generics
import TreeSitter.Language
import TreeSitter.Node
import TreeSitter.Struct

data TSQuery

data TSQueryCursor

type TSQueryError = Int
-- TODO: Need help implementing storable
-- data TSQueryError = TSQueryErrorNone | TSQueryErrorSyntax | TSQueryErrorNodeType | TSQueryErrorField | TSQueryErrorCapture
--   deriving (Show, Enum)

data TSQueryCapture = TSQueryCapture
  { captureTSNode :: !TSNode
  , index :: !Word32
  } deriving (Show, Eq, Generic)

data TSQueryMatch = TSQueryMatch
  { id :: !Word32
  , pattern_index :: !Word16
  , capture_count :: !Word16
  , captures :: Ptr TSQueryCapture
  } deriving (Show, Eq, Generic)

instance Storable TSQueryMatch where
  alignment _ = alignment (0 :: Int32)
  {-# INLINE alignment #-}
  sizeOf _ = 80
  {-# INLINE sizeOf #-}
  peek =
    evalStruct $
    TSQueryMatch <$> peekStruct <*> peekStruct <*> peekStruct <*> peekStruct
  {-# INLINE peek #-}
  poke ptr (TSQueryMatch a b c d) =
    flip evalStruct ptr $ do
      pokeStruct a
      pokeStruct b
      pokeStruct c
      pokeStruct d
  {-# INLINE poke #-}

instance Storable TSQueryCapture where
  alignment _ = alignment (0 :: Int32)
  {-# INLINE alignment #-}
  sizeOf _ = 8
  {-# INLINE sizeOf #-}
  peek = evalStruct $ TSQueryCapture <$> peekStruct <*> peekStruct
  {-# INLINE peek #-}
  poke ptr (TSQueryCapture a b) =
    flip evalStruct ptr $ do
      pokeStruct a
      pokeStruct b
  {-# INLINE poke #-}

foreign import ccall safe "ts_query_new" ts_query_new ::
               Ptr Language ->
                 CString ->
                   Int -> Ptr Word32 -> Ptr TSQueryError -> IO (Ptr TSQuery)

foreign import ccall safe "ts_query_cursor_new" ts_query_cursor_new
               :: IO (Ptr TSQueryCursor)

foreign import ccall safe "src/bridge.c ts_query_cursor_exec_p"
               ts_query_cursor_exec_p ::
               Ptr TSQueryCursor -> Ptr TSQuery -> Ptr Node -> IO ()

foreign import ccall safe "ts_query_cursor_next_match"
               ts_query_cursor_next_match ::
               Ptr TSQueryCursor -> Ptr TSQueryMatch -> IO Bool
