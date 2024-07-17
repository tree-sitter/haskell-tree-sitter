module TreeSitter.R
( tree_sitter_r
, getNodeTypesPath
, getTestCorpusDir
) where

import Foreign.Ptr
import TreeSitter.Language
import Paths_tree_sitter_r

foreign import ccall unsafe "vendor/tree-sitter-r/src/parser.c tree_sitter_r" tree_sitter_r :: Ptr Language

getNodeTypesPath :: IO FilePath
getNodeTypesPath = getDataFileName "vendor/tree-sitter-r/src/node-types.json"

getTestCorpusDir :: IO FilePath
getTestCorpusDir = getDataFileName "vendor/tree-sitter-r/corpus"
