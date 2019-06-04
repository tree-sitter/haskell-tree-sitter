module TreeSitter.Ptr (TreeSitter_Ptr (..), ts_ptr_init, funptr_ts_ptr_free, ts_ptr_goto_first_child, ts_ptr_goto_next_sibling, ts_ptr_current_type) where

import Foreign.C
import Foreign.Ptr
import Foreign.Storable

import TreeSitter.Tree

foreign import ccall unsafe "src/bridge.c sizeof_tsnode" sizeof_tsnode :: CSize
foreign import ccall unsafe "src/bridge.c sizeof_tstreecursor" sizeof_tstreecursor :: CSize

newtype TreeSitter_Ptr = TreeSitter_Ptr ()

instance Storable TreeSitter_Ptr where
    alignment _ = alignment nullPtr
    sizeOf _    = fromIntegral (sizeof_tsnode + sizeof_tstreecursor)
    peek _ = error "Cant peek"
    poke _ _ = error "Cant poke"

foreign import ccall ts_ptr_init :: Ptr Tree -> Ptr TreeSitter_Ptr -> IO ()
foreign import ccall ts_ptr_goto_first_child :: Ptr TreeSitter_Ptr -> IO ()
foreign import ccall ts_ptr_goto_next_sibling :: Ptr TreeSitter_Ptr -> IO ()
foreign import ccall ts_ptr_current_type :: Ptr TreeSitter_Ptr -> IO CString
foreign import ccall "&ts_ptr_free" funptr_ts_ptr_free :: FunPtr (Ptr TreeSitter_Ptr -> IO ())
