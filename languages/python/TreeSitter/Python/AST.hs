{-# LANGUAGE DeriveAnyClass, DeriveGeneric, DuplicateRecordFields, TemplateHaskell, TypeApplications #-}
module TreeSitter.Python.AST where

import CodeGen.GenerateSyntax
import Prelude hiding (True, False, Float, Integer, String)
import qualified TreeSitter.Python as Grammar

astDeclarationsForLanguage Grammar.tree_sitter_python "../../vendor/tree-sitter-python/src/node-types.json"
