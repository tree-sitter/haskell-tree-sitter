module TreeSitter.TSX
( tree_sitter_tsx
) where

import Foreign.Ptr
import TreeSitter.Language

foreign import ccall unsafe "vendor/tree-sitter-typescript/tsx/src/parser.c tree_sitter_tsx" tree_sitter_tsx :: Ptr Language
