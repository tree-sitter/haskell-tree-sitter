module TreeSitter.Rust
( tree_sitter_rust
, getNodeTypesPath
, getTestCorpusDir
) where

import Foreign.Ptr
import TreeSitter.Language
import Paths_tree_sitter_rust

foreign import ccall unsafe "vendor/tree-sitter-rust/src/parser.c tree_sitter_rust" tree_sitter_rust :: Ptr Language

getNodeTypesPath :: IO FilePath
getNodeTypesPath = getDataFileName "vendor/tree-sitter-rust/src/node-types.json"

getTestCorpusDir :: IO FilePath
getTestCorpusDir = getDataFileName "vendor/tree-sitter-rust/corpus"
