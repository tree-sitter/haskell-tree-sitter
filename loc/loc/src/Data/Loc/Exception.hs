module Data.Loc.Exception
  ( LocException (..)
  ) where

import Data.Loc.Internal.Prelude

data LocException
  = EmptySpan
  deriving (Eq, Ord)

instance Exception LocException

instance Show LocException where
  showsPrec _ EmptySpan = showString "empty Span"
