module TreeSitter.TSX
( tree_sitter_tsx
, getNodeTypesPath
) where

import Foreign.Ptr
import TreeSitter.Language
import Paths_tree_sitter_tsx

foreign import ccall unsafe "vendor/tree-sitter-typescript/tsx/src/parser.c tree_sitter_tsx" tree_sitter_tsx :: Ptr Language

getNodeTypesPath :: IO FilePath
getNodeTypesPath = getDataFileName "vendor/tree-sitter-typescript/tsx/src/node-types.json"
