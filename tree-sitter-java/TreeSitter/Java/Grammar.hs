{-# LANGUAGE TemplateHaskell #-}
module TreeSitter.Java.Grammar
( Grammar(..)
) where

import Language.Haskell.TH
import TreeSitter.Java
import TreeSitter.Language

-- Regenerate template haskell code when these files change:
addDependentFileRelative "../../vendor/tree-sitter-java/src/parser.c"

-- | Statically-known rules corresponding to symbols in the grammar.
mkSymbolDatatype (mkName "Grammar") tree_sitter_java
