module TreeSitter.C
( tree_sitter_c
, getNodeTypesPath
, getTestCorpusDir
) where

import Foreign.Ptr
import TreeSitter.Language
import Paths_tree_sitter_c

foreign import ccall unsafe "vendor/tree-sitter-c/src/parser.c tree_sitter_c" tree_sitter_c :: Ptr Language

getNodeTypesPath :: IO FilePath
getNodeTypesPath = getDataFileName "vendor/tree-sitter-c/src/node-types.json"

getTestCorpusDir :: IO FilePath
getTestCorpusDir = getDataFileName "vendor/tree-sitter-c/test/corpus"
