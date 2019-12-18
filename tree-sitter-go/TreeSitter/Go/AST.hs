{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}

module TreeSitter.Go.AST
( module TreeSitter.Go.AST
) where

import TreeSitter.GenerateSyntax
import Prelude hiding (True, False, Float, Integer, String, Rational)
import qualified TreeSitter.Go as Grammar

astDeclarationsForLanguage Grammar.tree_sitter_go "../../vendor/tree-sitter-go/src/node-types.json"
