{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}

module TreeSitter.TypeScript.AST
( module TreeSitter.TypeScript.AST
) where

import TreeSitter.GenerateSyntax
import Prelude hiding (True, False, Float, Integer, String)
import qualified TreeSitter.TypeScript as Grammar

astDeclarationsForLanguage Grammar.tree_sitter_typescript "../../vendor/tree-sitter-typescript/typescript/src/node-types.json"
