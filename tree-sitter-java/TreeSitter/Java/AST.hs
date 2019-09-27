{-# LANGUAGE DeriveAnyClass, DeriveGeneric, DeriveTraversable, DuplicateRecordFields, TemplateHaskell, TypeApplications #-}
module TreeSitter.Java.AST where

import TreeSitter.GenerateSyntax
import Prelude hiding (True, False, Float, Integer, String)
import qualified TreeSitter.Java as Grammar
import Language.Haskell.TH

astDeclarationsForLanguage Grammar.tree_sitter_java "../../vendor/tree-sitter-java/src/node-types.json"
