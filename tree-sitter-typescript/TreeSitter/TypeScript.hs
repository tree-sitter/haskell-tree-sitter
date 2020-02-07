module TreeSitter.TypeScript
( tree_sitter_typescript
, getNodeTypesPath
) where

import Foreign.Ptr
import TreeSitter.Language
import Paths_tree_sitter_typescript

foreign import ccall unsafe "vendor/tree-sitter-typescript/typescript/src/parser.c tree_sitter_typescript" tree_sitter_typescript :: Ptr Language

getNodeTypesPath :: IO FilePath
getNodeTypesPath = getDataFileName "vendor/tree-sitter-typescript/typescript/src/node-types.json"
