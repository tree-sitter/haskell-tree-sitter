module TreeSitter.Nix (
  tree_sitter_nix
) where

import Foreign.Ptr
import TreeSitter.Language

foreign import ccall unsafe "vendor/tree-sitter-nix/src/parser.c tree_sitter_nix" tree_sitter_nix :: Ptr Language
