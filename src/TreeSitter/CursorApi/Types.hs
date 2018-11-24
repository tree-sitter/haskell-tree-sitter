module TreeSitter.CursorApi.Types (
  SpanInfo(..)
)
where

import Data.Loc.Span

data SpanInfo = Parent Span | Token Span
  deriving (Show, Eq, Ord)
