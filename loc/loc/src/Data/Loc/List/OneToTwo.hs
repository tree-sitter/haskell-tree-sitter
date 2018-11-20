{-# LANGUAGE DeriveFoldable, DeriveFunctor, LambdaCase #-}

module Data.Loc.List.OneToTwo
  (
  -- * Imports
  -- $imports

  -- * Type
    OneToTwo (..)

  -- * Tuple conversion
  , toTuple
  , toTuple'
  ) where

import Data.Loc.Internal.Prelude

-- | List of length 1 or 2.
data OneToTwo a
  = One a   -- ^ List of length 1
  | Two a a -- ^ List of length 2
  deriving (Eq, Ord, Show, Read, Foldable, Functor)

{- |

>>> toTuple (One 1)
(1,Nothing)

>>> toTuple (Two 1 2)
(1,Just 2)

-}
toTuple :: OneToTwo a -> (a, Maybe a)
toTuple =
  \case
    One a -> (a, Nothing)
    Two a b -> (a, Just b)

{- |

>>> toTuple' (One 1)
(Nothing,1)

>>> toTuple' (Two 1 2)
(Just 1,2)

-}
toTuple' :: OneToTwo a -> (Maybe a, a)
toTuple' =
  \case
    One a -> (Nothing, a)
    Two a b -> (Just a, b)

{- $imports

Recommended import:

> import Data.Loc.List.OneToTwo (OneToTwo)
> import qualified Data.Loc.List.OneToTwo as OneToTwo

-}
