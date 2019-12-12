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
, module TreeSitter.JSON.AST.Internal
, (:+:)(..)
) where

import GHC.Generics ((:+:)(..))
import Prelude hiding (String)
import TreeSitter.GenerateSyntax
import TreeSitter.JSON.AST.Internal
import qualified TreeSitter.JSON as Grammar

astDeclarationsForLanguage Grammar.tree_sitter_json [''StringContent] "../../vendor/tree-sitter-json/src/node-types.json"
