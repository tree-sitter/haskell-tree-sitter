module TreeSitter.Scala
( tree_sitter_scala
, getNodeTypesPath
, getTestCorpusDir
) where

import Foreign.Ptr
import TreeSitter.Language
import Paths_tree_sitter_scala

foreign import ccall unsafe "vendor/tree-sitter-scala/src/parser.c tree_sitter_scala" tree_sitter_scala :: Ptr Language

getNodeTypesPath :: IO FilePath
getNodeTypesPath = getDataFileName "vendor/tree-sitter-scala/src/node-types.json"

getTestCorpusDir :: IO FilePath
getTestCorpusDir = getDataFileName "vendor/tree-sitter-scala/corpus"
