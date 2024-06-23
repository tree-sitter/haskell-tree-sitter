module TreeSitter.OCaml
( tree_sitter_ocaml
, getNodeTypesPath
, getTestCorpusDir
) where

import Foreign.Ptr
import TreeSitter.Language
import Paths_tree_sitter_ocaml

foreign import ccall unsafe "vendor/tree-sitter-ocaml/grammars/ocaml/src/parser.c tree_sitter_ocaml" tree_sitter_ocaml :: Ptr Language

getNodeTypesPath :: IO FilePath
getNodeTypesPath = getDataFileName "vendor/tree-sitter-ocaml/grammars/ocaml/src/node-types.json"

getTestCorpusDir :: IO FilePath
getTestCorpusDir = getDataFileName "vendor/tree-sitter-ocaml/grammars/ocaml/test/corpus"
