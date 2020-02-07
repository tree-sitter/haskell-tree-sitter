module TreeSitter.Ruby
( tree_sitter_ruby
, getNodeTypesPath
) where

import Foreign.Ptr
import TreeSitter.Language
import Paths_tree_sitter_ruby

foreign import ccall unsafe "vendor/tree-sitter-ruby/src/parser.c tree_sitter_ruby" tree_sitter_ruby :: Ptr Language

getNodeTypesPath :: IO FilePath
getNodeTypesPath = getDataFileName "vendor/tree-sitter-ruby/src/node-types.json"
