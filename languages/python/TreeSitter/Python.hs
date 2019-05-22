{-# LANGUAGE DeriveAnyClass, DeriveGeneric, DuplicateRecordFields, TemplateHaskell #-}
module TreeSitter.Python where

import Foreign.Ptr
import TreeSitter.Language

foreign import ccall unsafe "vendor/tree-sitter-python/src/parser.c tree_sitter_python" tree_sitter_python :: Ptr Language
