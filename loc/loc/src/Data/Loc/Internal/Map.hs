module Data.Loc.Internal.Map
  ( module Data.Map
  , below, above, belowInclusive, aboveInclusive
  ) where

import Data.Loc.Internal.Prelude

import Data.Map

{- |

@'below' k m@ is the subset of 'Map' @m@ whose keys are less than @k@.

-}
below :: Ord k => k -> Map k a -> Map k a
below k m =
  let
    (x, _) = split k m
  in
    x

{- |

@'below' k m@ is the subset of 'Map' @m@ whose keys are greater than @k@.

-}
above :: Ord k => k -> Map k a -> Map k a
above k m =
  let
    (_, x) = split k m
  in
    x

{- |

@'belowInclusive' k m@ is the subset of 'Map' @m@ whose keys are less than or
equal to @k@.

-}
belowInclusive :: Ord k => k -> Map k a -> Map k a
belowInclusive k m =
  let
    (x, at, _) = splitLookup k m
  in
    case at of
      Nothing -> x
      Just v -> insert k v x

{- |

@'aboveInclusive' k m@ is the subset of 'Map' @m@ whose keys are greater than
or equal to @k@.

-}
aboveInclusive :: Ord k => k -> Map k a -> Map k a
aboveInclusive k m =
  let
    (_, at, x) = splitLookup k m
  in
    case at of
      Nothing -> x
      Just v -> insert k v x
