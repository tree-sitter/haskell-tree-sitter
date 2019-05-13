{-# LANGUAGE TemplateHaskell, DuplicateRecordFields #-}
module TreeSitter.Ruby
( tree_sitter_ruby
) where

import Foreign.Ptr
import TreeSitter.Language
import CodeGen.GenerateSyntax
import Control.Monad.IO.Class

foreign import ccall unsafe "vendor/tree-sitter-ruby/src/parser.c tree_sitter_ruby" tree_sitter_ruby :: Ptr Language

$(input >>= liftIO . print >> pure [])
$(input >>= traverse datatypeForConstructors)
