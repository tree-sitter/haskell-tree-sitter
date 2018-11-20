module Data.Loc.Loc
  ( Loc

  -- * Constructing
  , loc
  , origin

  -- * Querying
  , line
  , column

  -- * Show and Read
  , locShowsPrec
  , locReadPrec

  ) where

import Data.Loc.Pos (Column, Line)

import Data.Loc.Internal.Prelude

{- |

Stands for /location/. Consists of a 'Line' and a 'Column'. You can think of a
'Loc' like a caret position in a text editor. Following the normal convention
for text editors and such, line and column numbers start with 1.

-}
data Loc = Loc
  { line   :: Line
  , column :: Column
  }
  deriving (Eq, Ord)

-- | 'showsPrec' = 'locShowsPrec'
instance Show Loc
  where

    showsPrec = locShowsPrec

-- | 'readPrec' = 'locReadPrec'
instance Read Loc
  where

    readPrec = locReadPrec

{- |

>>> locShowsPrec minPrec (loc 3 14) ""
"3:14"

-}
locShowsPrec :: Int -> Loc -> ShowS
locShowsPrec _ (Loc l c) =
  shows l .
  showString ":" .
  shows c

{- |

>>> readPrec_to_S locReadPrec minPrec "3:14"
[(3:14,"")]

-}
locReadPrec :: ReadPrec Loc
locReadPrec =
  Loc              <$>
  readPrec         <*
  readPrecChar ':' <*>
  readPrec

-- | Create a 'Loc' from a line number and column number.
loc :: Line -> Column -> Loc
loc = Loc

{- |

The smallest location: @'loc' 1 1@.

>>> origin
1:1

-}
origin :: Loc
origin = loc 1 1
