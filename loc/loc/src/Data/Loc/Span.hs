{-# LANGUAGE LambdaCase #-}

module Data.Loc.Span
  ( Span

  -- * Constructing
  , fromTo
  , fromToMay

  -- * Querying
  , start
  , end

  -- * Calculations
  , lines
  , overlapping
  , linesOverlapping
  , touching
  , join
  , joinAsc
  , (+)
  , (-)

  -- * Show and Read
  , spanShowsPrec
  , spanReadPrec

  ) where

import Data.Loc.Internal.Prelude
import Data.Loc.Loc (locReadPrec, locShowsPrec)

import Data.Loc.Exception (LocException (..))
import Data.Loc.List.OneToTwo (OneToTwo)
import Data.Loc.List.ZeroToTwo (ZeroToTwo)
import Data.Loc.Loc (Loc)
import Data.Loc.Pos (Line)

import qualified Data.Loc.List.OneToTwo as OneToTwo
import qualified Data.Loc.List.ZeroToTwo as ZeroToTwo
import qualified Data.Loc.Loc as Loc

import qualified Data.Foldable as Foldable
import qualified Data.List.NonEmpty as NonEmpty

{- |

A 'Span' consists of a start location ('start') and an end location ('end').
The end location must be greater than the start location; in other words, empty
or backwards spans are not permitted.

Construct and combine spans using 'fromTo', 'fromToMay', '+', and '-'.

-}
data Span = Span
  { start :: Loc
  , end   :: Loc
  } deriving (Eq, Ord)

-- | 'showsPrec' = 'spanShowsPrec'
instance Show Span
  where

    showsPrec = spanShowsPrec

-- | 'readPrec' = 'spanReadPrec'
instance Read Span
  where

    readPrec = spanReadPrec


{- |

>>> spanShowsPrec minPrec (fromTo (read "3:14") (read "6:5")) ""
"3:14-6:5"

-}
spanShowsPrec :: Int -> Span -> ShowS
spanShowsPrec _ (Span a b) =
  locShowsPrec 10 a .
  showString "-" .
  locShowsPrec 10 b

{- |

>>> readPrec_to_S spanReadPrec minPrec "3:14-6:5"
[(3:14-6:5,"")]

>>> readPrec_to_S spanReadPrec minPrec "6:5-3:14"
[(3:14-6:5,"")]

>>> readPrec_to_S spanReadPrec minPrec "6:5-6:5"
[]

-}
spanReadPrec :: ReadPrec Span
spanReadPrec =
  locReadPrec      >>= \a ->
  readPrecChar '-' *>
  locReadPrec      >>= \b ->
  maybe empty pure (fromToMay a b)

{- |

Attempt to construct a 'Span' from two 'Loc's. The lesser loc will be the
start, and the greater loc will be the end. The two locs must not be equal,
or else this throws 'EmptySpan'.

/The safe version of this function is 'fromToMay'./

-}
fromTo :: Loc -> Loc -> Span
fromTo a b =
  maybe (throw EmptySpan) id (fromToMay a b)

{- |

Attempt to construct a 'Span' from two 'Loc's. The lesser loc will be the
start, and the greater loc will be the end. If the two locs are equal,
the result is 'Nothing', because a span cannot be empty.

/This is the safe version of 'fromTo', which throws an exception instead./
-}
fromToMay :: Loc -> Loc -> Maybe Span
fromToMay a b =
  case compare a b of
    LT -> Just (Span a b)
    GT -> Just (Span b a)
    EQ -> Nothing

{- |

All of the lines that a span touches.

>>> NonEmpty.toList (lines (read "2:6-2:10"))
[2]

>>> NonEmpty.toList (lines (read "2:6-8:4"))
[2,3,4,5,6,7,8]

-}
lines :: Span -> NonEmpty Line
lines s =
  NonEmpty.fromList [Loc.line (start s) .. Loc.line (end s)]

{- |

Spans that are directly abutting do not count as overlapping.

>>> overlapping (read "1:5-1:8") (read "1:8-1:12")
False

But these spans overlap by a single character:

>>> overlapping (read "1:5-1:9") (read "1:8-1:12")
True

Spans are overlapping if one is contained entirely within another.

>>> overlapping (read "1:5-1:15") (read "1:6-1:10")
True

Spans are overlapping if they are identical.

>>> overlapping (read "1:5-1:15") (read "1:5-1:15")
True

-}
overlapping :: Span -> Span -> Bool
overlapping a b =
  not (end a <= start b || end b <= start a)

{- |

Determines whether the two spans touch any of the same lines.

>>> linesOverlapping (read "1:1-1:2") (read "1:1-1:2")
True

>>> linesOverlapping (read "1:1-1:2") (read "1:1-2:1")
True

>>> linesOverlapping (read "1:1-1:2") (read "2:1-2:2")
False

-}
linesOverlapping :: Span -> Span -> Bool
linesOverlapping a b =
  not $
    (Loc.line . end) a < (Loc.line . start) b ||
    (Loc.line . end) b < (Loc.line . start) a

{- |

Two spans are considered to "touch" if they are overlapping or abutting;
in other words, if there is no space between them.

>>> touching (read "1:1-1:2") (read "1:2-1:3")
True

>>> touching (read "1:1-1:2") (read "1:1-1:3")
True

>>> touching (read "1:1-1:2") (read "1:3-1:4")
False

-}
touching :: Span -> Span -> Bool
touching a b =
  not (end a < start b || end b < start a)

{- |

>>> join (read "1:1-1:2") (read "1:2-1:3")
1:1-1:3

>>> join (read "1:1-1:2") (read "1:1-1:3")
1:1-1:3

-}
join :: Span -> Span -> Span
join a b =
  Span (min (start a) (start b))
       (max (end   a) (end   b))

{- |

Combine two 'Span's, merging them if they abut or overlap.

>>> read "1:1-1:2" + read "1:2-1:3"
One 1:1-1:3

>>> read "1:1-1:2" + read "1:1-3:1"
One 1:1-3:1

>>> read "1:1-1:2" + read "1:1-11:1"
One 1:1-11:1

If the spans are not overlapping or abutting, they are returned unmodified
in the same order in which they were given as parameters.

>>> read "1:1-1:2" + read "2:1-2:5"
Two 1:1-1:2 2:1-2:5

>>> read "2:1-2:5" + read "1:1-1:2"
Two 2:1-2:5 1:1-1:2

-}
(+) :: Span -> Span -> OneToTwo Span
a + b
  | touching a b = OneToTwo.One (join a b)
  | otherwise    = OneToTwo.Two a b

{- |

The difference between two 'Spans's. @a '-' b@ contains what is covered by
@a@ and not covered by @b@.

>>> read "2:5-4:1" - read "2:9-3:5"
Two 2:5-2:9 3:5-4:1

>>> read "2:5-4:1" - read "2:5-3:5"
One 3:5-4:1

>>> read "2:5-4:1" - read "2:2-3:5"
One 3:5-4:1

Subtracting a thing from itself yields nothing.

>>> let x = read "2:5-4:1" in x - x
Zero

>>> read "2:5-4:1" - read "2:2-4:4"
Zero

>>> read "1:1-8:1" - read "1:2-8:1"
One 1:1-1:2

-}
(-) :: Span -> Span -> ZeroToTwo Span
a - b

    -- [   a   ]   [   b   ]
  | not (overlapping a b) =
      ZeroToTwo.One a

    -- [   a   ]
    --   [ b ]
  | start b > start a && end b < end a =
      ZeroToTwo.Two (Span (start a) (start b))
                    (Span (end b) (end a))

    --    [   a   ]
    -- [   b    ]
  | start b <= start a && end b < end a =
      ZeroToTwo.One (Span (end b) (end a))

    -- [   a   ]
    --    [   b   ]
  | start b > start a && end b >= end a =
      ZeroToTwo.One (Span (start a) (start b))

  | otherwise =
      ZeroToTwo.Zero

-- | Given an ascending list of 'Span's, combine those which abut or overlap.
joinAsc
  :: [Span] -- ^ A list of 'Spans' sorted in ascending order.
            --
            -- /This precondition is not checked./
  -> [Span]
joinAsc =
  \case
    x:y:zs ->
      let (r, s) = OneToTwo.toTuple' (x + y)
      in  Foldable.toList r <> joinAsc (s:zs)
    xs -> xs
