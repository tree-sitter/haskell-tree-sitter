{-# LANGUAGE CPP, DataKinds, FlexibleInstances, UndecidableInstances, FlexibleInstances, FlexibleContexts, MonoLocalBinds #-}
module TreeSitter.Python
( tree_sitter_python
, getNodeTypesPath
, getTestCorpusDir
) where

import Foreign.Ptr
import TreeSitter.Language
#if BAZEL_BUILD
import GHC.TypeLits
#else
import Paths_tree_sitter_python
#endif

foreign import ccall unsafe "vendor/tree-sitter-python/src/parser.c tree_sitter_python" tree_sitter_python :: Ptr Language

#if BAZEL_BUILD
class Disallow a

instance TypeError (Text "Can't call this function under Bazel") => Disallow a


getNodeTypesPath :: Disallow FilePath => IO FilePath
getNodeTypesPath = error "BUG: shouldn't be able to call getNodeTypesPath"

getTestCorpusDir :: Disallow FilePath => IO FilePath
getTestCorpusDir = error "BUG: shouldn't be able to call getTestCorpusDir"
#else
getNodeTypesPath :: IO FilePath
getNodeTypesPath = getDataFileName "vendor/tree-sitter-python/src/node-types.json"

getTestCorpusDir :: IO FilePath
getTestCorpusDir = getDataFileName "vendor/tree-sitter-python/test/corpus"
#endif
