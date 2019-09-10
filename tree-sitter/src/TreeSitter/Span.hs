module TreeSitter.Span
( Span(..)
, Pos(..)
) where


-- | Source position information
data Pos = Pos
  { posLine   :: {-# UNPACK #-} !Int
  , posColumn :: {-# UNPACK #-} !Int
  } deriving (Eq, Ord, Show)

-- | A Span of position information
data Span = Span
  { spanStart :: {-# UNPACK #-} !Pos
  , spanEnd   :: {-# UNPACK #-} !Pos
  } deriving (Eq, Ord, Show)
