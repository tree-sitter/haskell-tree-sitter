{-# LANGUAGE DeriveAnyClass, DeriveGeneric, DuplicateRecordFields, TemplateHaskell #-}
module TreeSitter.Python where

import Foreign.Ptr
import TreeSitter.Language
import CodeGen.Deserialize
import CodeGen.GenerateSyntax
import Control.Monad.IO.Class
import Language.Haskell.TH
import Data.Aeson as Aeson

foreign import ccall unsafe "vendor/tree-sitter-python/src/parser.c tree_sitter_python" tree_sitter_python :: Ptr Language

-- Read JSON as input
input :: Q [MkDatatype]
input = liftIO (eitherDecodeFileStrict' "./src/node-types.json") >>= either fail pure
