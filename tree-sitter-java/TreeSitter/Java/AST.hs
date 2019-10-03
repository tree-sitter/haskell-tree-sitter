{-# LANGUAGE DataKinds, DeriveAnyClass, DeriveGeneric, DeriveTraversable, DuplicateRecordFields, TemplateHaskell, TypeApplications #-}
module TreeSitter.Java.AST
( module TreeSitter.Java.AST
) where

import           TreeSitter.GenerateSyntax
import           TreeSitter.Java (tree_sitter_java)
import qualified TreeSitter.Java.Grammar as Grammar

astDeclarationsForLanguage tree_sitter_java "../../vendor/tree-sitter-java/src/node-types.json"
