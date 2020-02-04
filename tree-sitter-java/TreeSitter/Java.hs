module TreeSitter.Java
( tree_sitter_java
, getNodeTypesPath
) where

import Foreign.Ptr
import TreeSitter.Language
import Paths_tree_sitter_java

foreign import ccall unsafe "vendor/tree-sitter-java/src/parser.c tree_sitter_java" tree_sitter_java :: Ptr Language

getNodeTypesPath :: IO FilePath
getNodeTypesPath = getDataFileName "vendor/tree-sitter-Java/Java/src/node-types.json"