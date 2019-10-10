{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}

module TreeSitter.Python.AST
( module TreeSitter.Python.AST
) where

import TreeSitter.GenerateSyntax
import Prelude hiding (True, False, Float, Integer, String)
import qualified TreeSitter.Python as Grammar

astDeclarationsForLanguage Grammar.tree_sitter_python "../../vendor/tree-sitter-python/src/node-types.json"
