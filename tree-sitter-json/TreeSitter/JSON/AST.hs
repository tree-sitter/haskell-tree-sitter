{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}

module TreeSitter.JSON.AST
( module TreeSitter.JSON.AST
) where

import           Prelude hiding (String)
import           TreeSitter.GenerateSyntax
import qualified TreeSitter.JSON as Grammar

astDeclarationsForLanguage Grammar.tree_sitter_json "../../vendor/tree-sitter-json/src/node-types.json"
