module TreeSitter.Go
( tree_sitter_go
, getNodeTypesPath
) where

import Foreign.Ptr
import TreeSitter.Language
import Paths_tree_sitter_go

foreign import ccall unsafe "vendor/tree-sitter-go/src/parser.c tree_sitter_go" tree_sitter_go :: Ptr Language

getNodeTypesPath :: IO FilePath
getNodeTypesPath = getDataFileName "vendor/tree-sitter-go/src/node-types.json"
