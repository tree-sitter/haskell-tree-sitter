module TreeSitter.Python
( tree_sitter_python
, getNodeTypesPath
) where

import Foreign.Ptr
import TreeSitter.Language
import Paths_tree_sitter_python

foreign import ccall unsafe "vendor/tree-sitter-python/src/parser.c tree_sitter_python" tree_sitter_python :: Ptr Language

getNodeTypesPath :: IO FilePath
getNodeTypesPath = getDataFileName "vendor/tree-sitter-python/src/node-types.json"
