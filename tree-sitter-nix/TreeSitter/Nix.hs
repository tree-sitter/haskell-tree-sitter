{-# LANGUAGE TemplateHaskell #-}
module TreeSitter.Nix (
  tree_sitter_nix
, Grammar(..)
) where

import Language.Haskell.TH
import TreeSitter.Language
import TreeSitter.Nix.Internal

-- Regenerate template haskell code when these files change:
addDependentFileRelative "../vendor/tree-sitter-nix/src/parser.c"

-- | Statically-known rules corresponding to symbols in the grammar.
mkSymbolDatatype (mkName "Grammar") tree_sitter_nix
