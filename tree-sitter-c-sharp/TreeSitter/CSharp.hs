module TreeSitter.CSharp
( tree_sitter_c_sharp
, getNodeTypesPath
, getTestCorpusDir
) where

import Foreign.Ptr
import TreeSitter.Language
import Paths_tree_sitter_c_sharp

foreign import ccall unsafe "vendor/tree-sitter-c-sharp/src/parser.c tree_sitter_c_sharp" tree_sitter_c_sharp :: Ptr Language

getNodeTypesPath :: IO FilePath
getNodeTypesPath = getDataFileName "vendor/tree-sitter-c-sharp/src/node-types.json"

getTestCorpusDir :: IO FilePath
getTestCorpusDir = getDataFileName "vendor/tree-sitter-c-sharp/corpus"
