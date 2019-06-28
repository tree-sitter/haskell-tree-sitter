{-# LANGUAGE TemplateHaskell #-}
module TreeSitter.Haskell
( tree_sitter_haskell
, Grammar(..)
) where

import Language.Haskell.TH
import TreeSitter.Haskell.Internal
import TreeSitter.Language

-- Regenerate template haskell code when these files change:
addDependentFileRelative "../vendor/tree-sitter-haskell/src/parser.c"

-- | Statically-known rules corresponding to symbols in the grammar.
mkSymbolDatatype (mkName "Grammar") tree_sitter_haskell
