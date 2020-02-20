module TreeSitter.JSON
( tree_sitter_json
, getNodeTypesPath
, getTestCorpusDir
) where

import Foreign.Ptr
import TreeSitter.Language
import Paths_tree_sitter_json

foreign import ccall unsafe "vendor/tree-sitter-json/src/parser.c tree_sitter_json" tree_sitter_json :: Ptr Language

getNodeTypesPath :: IO FilePath
getNodeTypesPath = getDataFileName "vendor/tree-sitter-json/src/node-types.json"

getTestCorpusDir :: IO FilePath
getTestCorpusDir = getDataFileName "vendor/tree-sitter-json/corpus"
