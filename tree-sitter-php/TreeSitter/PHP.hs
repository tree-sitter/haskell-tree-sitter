{-# LANGUAGE TemplateHaskell #-}
module TreeSitter.PHP
( tree_sitter_php
, Grammar(..)
) where

import Language.Haskell.TH
import TreeSitter.PHP.Internal
import TreeSitter.Language

-- Regenerate template haskell code when these files change:
addDependentFileRelative "../vendor/tree-sitter-php/src/parser.c"

-- | Statically-known rules corresponding to symbols in the grammar.
mkSymbolDatatype (mkName "Grammar") tree_sitter_php
