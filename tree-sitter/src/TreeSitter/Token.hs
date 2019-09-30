{-# LANGUAGE DeriveGeneric, DeriveTraversable, PolyKinds #-}
module TreeSitter.Token
( Token(..)
) where

import GHC.Generics (Generic, Generic1)

newtype Token symName symVal a = Token { ann :: a }
  deriving (Eq, Foldable, Functor, Generic, Generic1, Ord, Show, Traversable)