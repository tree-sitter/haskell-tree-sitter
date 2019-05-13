{-# LANGUAGE TemplateHaskell, DuplicateRecordFields #-}
module TreeSitter.CodeGen where

import CodeGen.GenerateSyntax
import Control.Monad.IO.Class
import Language.Haskell.TH
import TreeSitter.Python

-- Auto-generate code from node-types.json
$(input >>= liftIO . print >> pure [])
$(input >>= traverse datatypeForConstructors)
