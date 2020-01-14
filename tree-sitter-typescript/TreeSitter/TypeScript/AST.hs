{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module TreeSitter.TypeScript.AST
( module TreeSitter.TypeScript.AST
) where

import           Prelude hiding (False, Float, Integer, String, True)
import           TreeSitter.GenerateSyntax
import qualified TreeSitter.TypeScript as Grammar

astDeclarationsForLanguage Grammar.tree_sitter_typescript "../../vendor/tree-sitter-typescript/typescript/src/node-types.json"
