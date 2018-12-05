module TreeSitter.CursorApi.Types (
  SpanInfo(..)
  , spanInfoRange
)
where

-- TODO Text instead of String ?
data SpanInfo =
  Parent Int Int String
  | Token Int Int String
  | Error Int Int
  deriving (Show, Eq, Ord)

spanInfoRange :: SpanInfo -> (Int, Int)
spanInfoRange (Parent start end _) = (start, end)
spanInfoRange (Token start end _) = (start, end)
spanInfoRange (Error start end) = (start, end)