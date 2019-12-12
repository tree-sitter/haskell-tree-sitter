{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}

module TreeSitter.Java.AST
( module TreeSitter.Java.AST
) where

import TreeSitter.GenerateSyntax
import qualified TreeSitter.Java as Grammar

astDeclarationsForLanguage Grammar.tree_sitter_java [] =<< pathRelativeToCurrentModule "../../vendor/tree-sitter-java/src/node-types.json"
