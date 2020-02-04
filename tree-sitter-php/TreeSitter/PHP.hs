module TreeSitter.PHP
( tree_sitter_php
, getNodeTypesPath
) where

import Foreign.Ptr
import TreeSitter.Language
import Paths_tree_sitter_php

foreign import ccall unsafe "vendor/tree-sitter-php/src/parser.c tree_sitter_php" tree_sitter_php :: Ptr Language

getNodeTypesPath :: IO FilePath
getNodeTypesPath = getDataFileName "vendor/tree-sitter-php/php/src/node-types.json"