{-# LANGUAGE DataKinds, DeriveAnyClass, DeriveGeneric, DeriveTraversable, DuplicateRecordFields, TemplateHaskell, TypeApplications, GeneralizedNewtypeDeriving, DerivingStrategies #-}
module TreeSitter.JSON.AST
( module TreeSitter.JSON.AST
, (:+:)(..)
) where

import qualified Data.Text as Text
import GHC.Generics (Generic, Generic1, (:+:)(..))
import Prelude hiding (String)
import TreeSitter.GenerateSyntax
import qualified TreeSitter.JSON as Grammar
import qualified TreeSitter.Unmarshal as TS

-- | The content of a string literal.
--
-- This is defined rather than being derived from the grammar to represent it as just the textual content, without explicit representation of escape sequences.
data StringContent a = StringContent
  { ann  :: a
  , text :: Text.Text
  }
  deriving (Eq, Foldable, Functor, Generic, Generic1, Ord, Show, Traversable, TS.Unmarshal)

astDeclarationsForLanguage Grammar.tree_sitter_json "../../vendor/tree-sitter-json/src/node-types.json"
