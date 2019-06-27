{-# LANGUAGE TemplateHaskell #-}
module TreeSitter.TSX
( tree_sitter_tsx
, Grammar(..)
) where

import Language.Haskell.TH
import TreeSitter.TSX.Internal
import TreeSitter.Language

-- Regenerate template haskell code when these files change:
addDependentFileRelative "../vendor/tree-sitter-typescript/tsx/src/parser.c"

-- | Statically-known rules corresponding to symbols in the grammar.
mkSymbolDatatype (mkName "Grammar") tree_sitter_tsx
