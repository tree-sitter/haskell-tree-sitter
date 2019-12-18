{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}

module TreeSitter.Ruby.AST
( module TreeSitter.Ruby.AST
) where

import TreeSitter.GenerateSyntax
import Prelude hiding (True, False, Float, Integer, String, Rational)
import qualified TreeSitter.Ruby as Grammar

astDeclarationsForLanguage Grammar.tree_sitter_ruby "../../vendor/tree-sitter-ruby/src/node-types.json"
