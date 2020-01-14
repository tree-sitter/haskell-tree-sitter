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

module TreeSitter.Go.AST
( module TreeSitter.Go.AST
) where

import           Prelude hiding (False, Float, Integer, Rational, String, True)
import           TreeSitter.GenerateSyntax
import qualified TreeSitter.Go as Grammar

astDeclarationsForLanguage Grammar.tree_sitter_go "../../vendor/tree-sitter-go/src/node-types.json"
