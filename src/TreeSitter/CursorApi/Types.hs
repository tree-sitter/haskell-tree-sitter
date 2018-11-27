module TreeSitter.CursorApi.Types (
  SpanInfo(..)
)
where

import Data.Loc.Span

data SpanInfo = Parent String Span | Token String Span
  deriving (Show, Eq, Ord)
