module TreeSitter.CursorApi.Types (
  SpanInfo(..)
)
where

type SourceOffsetFrom = Int
type SourceOffsetTo = Int

-- TODO Text instead of String ?
data SpanInfo = Parent SourceOffsetFrom SourceOffsetTo String | Token SourceOffsetFrom SourceOffsetTo String
  deriving (Show, Eq, Ord)
