{-# LANGUAGE DeriveAnyClass, DeriveGeneric, DeriveTraversable #-}
module TreeSitter.JSON.AST.Internal
( StringContent(..)
) where

import Data.Text as Text (Text)
import GHC.Generics (Generic, Generic1)
import TreeSitter.Unmarshal as TS

-- | The content of a string literal.
--
-- This is defined rather than being derived from the grammar to represent it as just the textual content, without explicit representation of escape sequences.
data StringContent a = StringContent
  { ann  :: a
  , text :: Text.Text
  }
  deriving (Eq, Foldable, Functor, Generic, Generic1, Ord, Show, Traversable, TS.Unmarshal)
