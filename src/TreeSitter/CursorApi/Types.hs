module TreeSitter.CursorApi.Types (
  SpanInfo(..)
)
where

-- TODO Text instead of String ?
data SpanInfo =
  Parent Int Int String
  | Token Int Int String
  | Error Int Int
  deriving (Show, Eq, Ord)
