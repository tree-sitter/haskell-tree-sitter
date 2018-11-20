{-# LANGUAGE GeneralizedNewtypeDeriving, ScopedTypeVariables #-}

module Data.Loc.Area
  ( Area

  -- * Constructing
  , fromTo
  , spanArea

  -- * Combining
  , (+)
  , (-)
  , addSpan

  -- * Querying
  , firstSpan
  , lastSpan
  , start
  , end
  , areaSpan
  , spansAsc
  , spanCount

  -- * Show and Read
  , areaShowsPrec
  , areaReadPrec

  ) where

import Data.Loc.Internal.Prelude

import Data.Loc.Loc (Loc)
import Data.Loc.Span (Span)

import qualified Data.Loc.Internal.Map as Map
import qualified Data.Loc.Span as Span

import qualified Data.Foldable as Foldable
import qualified Data.Set as Set

data Terminus = Start | End
  deriving (Eq, Ord)

{- |

A set of non-overlapping, non-abutting 'Span's. You may also think of an 'Area'
like a span that can be empty or have “gaps”.

Construct and combine areas using 'mempty', 'spanArea', 'fromTo', '+', and '-'.

-}
newtype Area = Area (Map Loc Terminus)
  deriving (Eq, Ord)

-- | 'showsPrec' = 'areaShowsPrec'
instance Show Area
  where

    showsPrec = areaShowsPrec

-- | 'readPrec' = 'areaReadPrec'
instance Read Area
  where

    readPrec = areaReadPrec

instance Monoid Area
  where

    mempty = Area Map.empty

    mappend = (+)

-- | '<>' = '+'
instance Semigroup Area
  where
    (<>) = (+)

areaShowsPrec :: Int -> Area -> ShowS
areaShowsPrec _ a =
  showList (spansAsc a)

{- |

>>> readPrec_to_S areaReadPrec minPrec "[]"
[([],"")]

>>> readPrec_to_S areaReadPrec minPrec "[3:2-5:5,8:3-11:4]"
[([3:2-5:5,8:3-11:4],"")]

>>> readPrec_to_S areaReadPrec minPrec "[3:2-5:5,11:4-8:3]"
[([3:2-5:5,8:3-11:4],"")]

>>> readPrec_to_S areaReadPrec minPrec "[3:2-5:5,8:3-8:3]"
[]

-}
areaReadPrec :: ReadPrec Area
areaReadPrec =
  foldMap spanArea <$> readListPrec

{- |

Construct a contiguous 'Area' consisting of a single 'Span' specified by two
'Loc's. The lesser loc will be the start, and the greater loc will be the end.
If the two locs are equal, the area will be empty.

-}
fromTo
  :: Loc -- ^ Start
  -> Loc -- ^ End
  -> Area
fromTo a b
  | a == b    = mempty
  | otherwise = spanArea (Span.fromTo a b)

{- |

Construct an 'Area' consisting of a single 'Span'.

>>> spanArea (read "4:5-6:3")
[4:5-6:3]

-}
spanArea :: Span -> Area
spanArea s = Area (Map.fromList locs)
  where
    locs = [ (Span.start s, Start)
           , (Span.end   s, End  )
           ]

{- |

A 'Span' from 'start' to 'end', or 'Nothing' if the 'Area' is empty.

>>> areaSpan mempty
Nothing

>>> areaSpan (read "[3:4-7:2]")
Just 3:4-7:2

>>> areaSpan (read "[3:4-7:2,15:6-17:9]")
Just 3:4-17:9

-}
areaSpan :: Area -> Maybe Span
areaSpan x =
  start x >>= \a ->
  end x   <&> \b ->
  Span.fromTo a b

{- |

A list of the 'Span's that constitute an 'Area', sorted in ascending order.

>>> spansAsc mempty
[]

>>> spansAsc (read "[3:4-7:2,15:6-17:9]")
[3:4-7:2,15:6-17:9]

-}
spansAsc :: Area -> [Span]
spansAsc (Area m) =
    mapAccumL f Nothing (Map.keys m) & snd & catMaybes
  where
    f Nothing  l  = (Just l,  Nothing)
    f (Just l) l' = (Nothing, Just $ Span.fromTo l l')

{- |

>>> spanCount mempty
0

>>> spanCount (read "[3:4-7:2]")
1

>>> spanCount (read "[3:4-7:2,15:6-17:9]")
2

-}
spanCount :: Area -> Natural
spanCount (Area locs) =
  fromIntegral (Foldable.length locs `div` 2)

{- |

The first contiguous 'Span' in the 'Area', or 'Nothing' if the area is empty.

>>> firstSpan mempty
Nothing

>>> firstSpan (read "[3:4-7:2]")
Just 3:4-7:2

>>> firstSpan (read "[3:4-7:2,15:6-17:9]")
Just 3:4-7:2

-}
firstSpan :: Area -> Maybe Span
firstSpan (Area m) =
  case Set.toAscList (Map.keysSet m) of
    a:b:_ -> Just (Span.fromTo a b)
    _     -> Nothing

{- |

The last contiguous 'Span' in the 'Area', or 'Nothing' if the area is empty.

>>> lastSpan mempty
Nothing

>>> lastSpan (read "[3:4-7:2]")
Just 3:4-7:2

>>> lastSpan (read "[3:4-7:2,15:6-17:9]")
Just 15:6-17:9

-}
lastSpan :: Area -> Maybe Span
lastSpan (Area m) =
  case Set.toDescList (Map.keysSet m) of
    b:a:_ -> Just (Span.fromTo a b)
    _     -> Nothing

{- |

The 'Loc' at which the 'Area' starts, or 'Nothing' if the 'Area' is empty.

>>> start mempty
Nothing

>>> start (read "[3:4-7:2]")
Just 3:4

>>> start (read "[3:4-7:2,15:6-17:9]")
Just 3:4

-}
start :: Area -> Maybe Loc
start (Area m) =
  case Map.minViewWithKey m of
    Just ((l, _), _) -> Just l
    Nothing          -> Nothing

{- |

The 'Loc' at which the 'Area' ends, or 'Nothing' if the 'Area' is empty.

>>> end mempty
Nothing

>>> end (read "[3:4-7:2]")
Just 7:2

>>> end (read "[3:4-7:2,15:6-17:9]")
Just 17:9

-}
end :: Area -> Maybe Loc
end (Area locs) =
  case Map.maxViewWithKey locs of
    Just ((l, _), _) -> Just l
    Nothing          -> Nothing

{- |

The union of two 'Area's. Spans that overlap or abut will be merged in the
result.

>>> read "[1:1-1:2]" + mempty
[1:1-1:2]

>>> read "[1:1-1:2]" + read "[1:2-1:3]"
[1:1-1:3]

>>> read "[1:1-1:2]" + read "[1:1-3:1]"
[1:1-3:1]

>>> read "[1:1-1:2]" + read "[1:1-11:1]"
[1:1-11:1]

>>> read "[1:1-3:1,6:1-6:2]" + read "[1:1-6:1]"
[1:1-6:2]

>>> read "[1:1-3:1]" + read "[5:1-6:2]"
[1:1-3:1,5:1-6:2]

-}
(+) :: Area -> Area -> Area
a + b
  | spanCount a >= spanCount b = foldr addSpan a (spansAsc b)
  | otherwise                  = b + a

{- |

@'addSpan' s a@ is the union of @'Area' a@ and @'Span' s@.

>>> addSpan (read "1:1-6:1") (read "[1:1-3:1,6:1-6:2]")
[1:1-6:2]

-}
addSpan :: Span -> Area -> Area
addSpan b (Area as) =

  let
    -- Spans lower than b that do not abut or overlap b.
    -- These spans will remain completely intact in the result.
    unmodifiedSpansBelow :: Map Loc Terminus

    -- Spans greater than b that do not abut or overlap b.
    -- These spans will remain completely intact in the result.
    unmodifiedSpansAbove :: Map Loc Terminus

    -- The start location of a span that starts below b but doesn't end below b,
    -- if such a span exists. This span will be merged into the 'middle'.
    startBelow :: Maybe Loc

    -- The end location of a span that ends above b but doesn't start above b,
    -- if such a span exists. This span will be merged into the 'middle'.
    endAbove :: Maybe Loc

    -- b, plus any spans it abuts or overlaps.
    middle :: Map Loc Terminus

    (unmodifiedSpansBelow, startBelow) =
      let
        below = Map.below (Span.start b) as
      in
        case Map.maxViewWithKey below of
          Just ((l, Start), xs) -> (xs, Just l)
          _ -> (below, Nothing)


    (unmodifiedSpansAbove, endAbove) =
      let
        above = Map.above (Span.end b) as
      in
        case Map.minViewWithKey above of
          Just ((l, End), xs) -> (xs, Just l)
          _ -> (above, Nothing)

    middle = Map.fromList
        [ (minimum $ Foldable.toList startBelow <> [Span.start b], Start)
        , (maximum $ Foldable.toList endAbove   <> [Span.end b],   End)
        ]

  in
    Area $ unmodifiedSpansBelow <> middle <> unmodifiedSpansAbove

{- |

The difference between two 'Area's. @a '-' b@ contains what is covered by @a@
and not covered by @b@.

-}
(-) :: Area -> Area -> Area
a - b = foldr subtractSpan a (spansAsc b)

{- |

@'subtractSpan' s a@ is the subset of 'Area' @a@ that is not covered by 'Span'
@s@.

-}
subtractSpan :: Span -> Area -> Area
subtractSpan b (Area as) =

  let
    resultBelow :: Map Loc Terminus =
      let
        below = Map.belowInclusive (Span.start b) as
      in
        case Map.maxViewWithKey below of
          Just ((l, Start), xs) ->
              if l == Span.start b
              then xs
              else below & Map.insert (Span.start b) End
          _ -> below

    resultAbove :: Map Loc Terminus =
      let
        above = Map.aboveInclusive (Span.end b) as
      in
        case Map.minViewWithKey above of
          Just ((l, End), xs) ->
              if l == Span.end b
              then xs
              else above & Map.insert (Span.end b) Start
          _ -> above

  in
    Area $ resultBelow <> resultAbove
