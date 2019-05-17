{-# LANGUAGE DeriveAnyClass, DeriveGeneric, DuplicateRecordFields, TemplateHaskell #-}
module TreeSitter.Python where

import Foreign.Ptr
import TreeSitter.Language
import CodeGen.GenerateSyntax
import Control.Monad.IO.Class
import Data.Aeson
import Prelude hiding (Float, Integer, String)

foreign import ccall unsafe "vendor/tree-sitter-python/src/parser.c tree_sitter_python" tree_sitter_python :: Ptr Language

-- Auto-generate code from node-types.json
$(do
  input <- liftIO (eitherDecodeFileStrict' "./vendor/tree-sitter-python/src/node-types.json")
  either fail (traverse datatypeForConstructors) input)
