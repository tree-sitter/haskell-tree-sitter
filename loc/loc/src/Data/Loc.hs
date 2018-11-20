module Data.Loc
  (
  -- * Concepts
  -- $concepts

  -- * Imports
  -- $imports

  -- * Core types
    Line, Column, Loc, Span, Area

  -- * Constructing
  -- ** Loc
  , loc, origin
  -- ** Span
  , spanFromTo, spanFromToMay
  -- ** Area
  , areaFromTo, spanArea

  -- * Deconstructing
  -- ** Loc
  , locLine, locColumn
  -- ** Span
  , spanStart, spanEnd
  -- ** Area
  , areaStart, areaEnd, areaSpansAsc

  -- * Combining
  -- ** Span
  , spanUnion, spanDifference
  -- ** Area
  , areaUnion, areaDifference

  -- * Miscellaneous
  , Pos, OneToTwo, ZeroToTwo, ToNat (..), LocException (..)

  ) where

import Data.Loc.Internal.Prelude

import Data.Loc.Area (Area)
import Data.Loc.Exception (LocException (..))
import Data.Loc.List.OneToTwo (OneToTwo)
import Data.Loc.List.ZeroToTwo (ZeroToTwo)
import Data.Loc.Loc (Loc)
import Data.Loc.Pos (Column, Line, Pos, ToNat (..))
import Data.Loc.Span (Span)

import qualified Data.Loc.Area as Area
import qualified Data.Loc.Loc as Loc
import qualified Data.Loc.Span as Span

{- |
The smallest location: @'loc' 1 1@.

/This is an alias for 'Loc.origin'./
-}
origin :: Loc
origin = Loc.origin

{- |
Create a 'Loc' from a line number and column number.

/This is an alias for 'Loc.loc'./
-}
loc :: Line -> Column -> Loc
loc = Loc.loc

-- | /This is an alias for 'Loc.line'./
locLine :: Loc -> Line
locLine = Loc.line

-- | /This is an alias for 'Loc.column'./
locColumn :: Loc -> Column
locColumn = Loc.column

{- |
Attempt to construct a 'Span' from two 'Loc's. The lesser loc will be the
start, and the greater loc will be the end. The two locs must not be equal,
or else this throws 'EmptySpan'.

/The safe version of this function is 'spanFromToMay'./

/This is an alias for 'Span.fromTo'./
-}
spanFromTo :: Loc -> Loc -> Span
spanFromTo = Span.fromTo

{- |
Attempt to construct a 'Span' from two 'Loc's. The lesser loc will be the
start, and the greater loc will be the end. If the two locs are equal,
the result is 'Nothing', because a span cannot be empty.

/This is the safe version of 'spanFromTo', which throws an exception instead./

/This is an alias for 'Span.fromToMay'./
-}
spanFromToMay :: Loc -> Loc -> Maybe Span
spanFromToMay = Span.fromToMay

{- |
Construct a contiguous 'Area' consisting of a single 'Span' specified by two
'Loc's. The lesser loc will be the start, and the greater loc will be the end.
If the two locs are equal, the area will be empty.

/This is an alias for 'Area.fromTo'./
-}
areaFromTo :: Loc -> Loc -> Area
areaFromTo = Area.fromTo

{- |
The union of two 'Area's. Spans that overlap or abut will be merged in the
result.

/This is an alias for 'Area.+'./
-}
areaUnion :: Area -> Area -> Area
areaUnion = (Area.+)

{- |
The difference between two 'Area's. @a `'areaDifference'` b@ contains what is
covered by @a@ and not covered by @b@.

/This is an alias for 'Area.-'./
-}
areaDifference :: Area -> Area -> Area
areaDifference = (Area.-)

{- |
A list of the 'Span's that constitute an 'Area', sorted in ascending order.

/This is an alias for 'Area.spansAsc'./
-}
areaSpansAsc :: Area -> [Span]
areaSpansAsc = Area.spansAsc

{- |
Construct an 'Area' consisting of a single 'Span'.

/This is an alias for 'Area.spanArea'./
-}
spanArea :: Span -> Area
spanArea = Area.spanArea

{- |
Combine two 'Span's, merging them if they abut or overlap.

/This is an alias for 'Span.+'./
-}
spanUnion :: Span -> Span -> OneToTwo Span
spanUnion = (Span.+)

{- |
The difference between two 'Spans's. @a '-' b@ contains what is covered by
@a@ and not covered by @b@.

/This is an alias for 'Span.-'./
-}
spanDifference :: Span -> Span -> ZeroToTwo Span
spanDifference = (Span.-)

{- |
/This is an alias for 'Span.start'./
-}
spanStart :: Span -> Loc
spanStart = Span.start

{- |
/This is an alias for 'Span.end'./
-}
spanEnd :: Span -> Loc
spanEnd = Span.end

{- |
/This is an alias for 'Area.start'./
-}
areaStart :: Area -> Maybe Loc
areaStart = Area.start

{- |
/This is an alias for 'Area.end'./
-}
areaEnd :: Area -> Maybe Loc
areaEnd = Area.end

{- $concepts

'Line' and 'Column' are positive integers representing line and column numbers.

The product of 'Line' and 'Column' is a 'Loc', which represents a position
between characters in multiline text. The smallest loc is 'origin': line 1,
column 1.

Here's a small piece of text for illustration:

>              1         2
>     12345678901234567890123456789
>   ┌───────────────────────────────┐
> 1 │ I have my reasons, you        │
> 2 │ have yours. What's obvious    │
> 3 │ to me isn't to everyone else, │
> 4 │ and vice versa.               │
>   └───────────────────────────────┘

In this example, the word “obvious” starts at line 2, column 20, and it ends at
line 2, column 27. The 'Show' instance uses a shorthand notation denoting
these locs as @2:20@ and @2:27@.

A 'Span' is a nonempty contiguous region of text between two locs; think of it
like a highlighted area in a simple text editor. In the above example, a span
that covers the word “obvious” starts at @2:20@ and ends at @2:27@. The 'Show'
instance describes this tersely as @2:20-2:27@.

Multiple non-overlapping regions form an 'Area'. You may also think of an
area like a span that can be empty or have “gaps”. In the example above, the
first three words “I have my”, and not the spaces between them, are covered by
the area @[1:1-1:2,1:3-1:7,1:8-1:10]@.

-}

{- $imports

Recommended import:

> import Data.Loc.Types
> import qualified Data.Loc as Loc

-}
