{-# LANGUAGE CPP, ForeignFunctionInterface, EmptyDataDecls #-}

#include "tree_sitter/runtime.h"
#include <tree_sitter_ptr.h>

#if __GLASGOW_HASKELL__ < 800
#let alignment t = "%lu", (unsigned long)offsetof(struct {char x__; t (y__); }, y__)
#endif

module TreeSitter.Ptr (TreeSitter_Ptr, ts_ptr_init, funptr_ts_ptr_free, ts_ptr_goto_first_child, ts_ptr_goto_next_sibling, ts_ptr_current_type) where

import Foreign.Ptr
import Foreign.Storable
import Foreign.C.String

import TreeSitter.Tree


data TreeSitter_Ptr

instance Storable TreeSitter_Ptr where
    alignment _ = #{alignment tree_sitter_ptr}
    sizeOf _ = #{size tree_sitter_ptr}
    peek _ = error "Cant peek"
    poke _ _ = error "Cant poke"

foreign import ccall ts_ptr_init :: Ptr Tree -> Ptr TreeSitter_Ptr -> IO ()
foreign import ccall ts_ptr_goto_first_child :: Ptr TreeSitter_Ptr -> IO ()
foreign import ccall ts_ptr_goto_next_sibling :: Ptr TreeSitter_Ptr -> IO ()
foreign import ccall ts_ptr_current_type :: Ptr TreeSitter_Ptr -> IO CString
foreign import ccall "&ts_ptr_free" funptr_ts_ptr_free :: FunPtr (Ptr TreeSitter_Ptr -> IO ())
