module TreeSitter.CursorApi.Types (
  SpanInfo(..)
)
where

import Data.Loc.Span

data SpanInfo = Parent Span String | Token Span String
  deriving (Show, Eq, Ord)
