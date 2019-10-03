{-# LANGUAGE DataKinds, DeriveAnyClass, DeriveGeneric, DeriveTraversable, DuplicateRecordFields, TemplateHaskell, TypeApplications #-}
module TreeSitter.Java.AST
( module TreeSitter.Java.AST
) where

import TreeSitter.GenerateSyntax
import qualified TreeSitter.Java as Grammar

astDeclarationsForLanguage Grammar.tree_sitter_java "../../vendor/tree-sitter-java/src/node-types.json"
