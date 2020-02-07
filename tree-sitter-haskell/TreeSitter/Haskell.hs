module TreeSitter.Haskell
( tree_sitter_haskell
, getNodeTypesPath
) where

import Foreign.Ptr
import TreeSitter.Language
import Paths_tree_sitter_haskell

foreign import ccall unsafe "vendor/tree-sitter-haskell/src/parser.c tree_sitter_haskell" tree_sitter_haskell :: Ptr Language

getNodeTypesPath :: IO FilePath
getNodeTypesPath = getDataFileName "vendor/tree-sitter-haskell/src/node-types.json"
