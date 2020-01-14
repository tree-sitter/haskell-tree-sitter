{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module TreeSitter.PHP.AST
( module TreeSitter.PHP.AST
) where

import           Prelude hiding (False, Float, Integer, String, True)
import           TreeSitter.GenerateSyntax
import qualified TreeSitter.PHP as Grammar

astDeclarationsForLanguage Grammar.tree_sitter_php "../../vendor/tree-sitter-php/src/node-types.json"
