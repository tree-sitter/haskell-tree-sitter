{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}

module TreeSitter.TSX.AST
( module TreeSitter.TSX.AST
) where

import TreeSitter.GenerateSyntax
import Prelude hiding (True, False, Float, Integer, String)
import qualified TreeSitter.TSX as Grammar

astDeclarationsForLanguage Grammar.tree_sitter_tsx "../../vendor/tree-sitter-typescript/tsx/src/node-types.json"
