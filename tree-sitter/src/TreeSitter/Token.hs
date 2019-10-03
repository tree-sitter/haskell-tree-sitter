{-# LANGUAGE DataKinds, DeriveGeneric, DeriveTraversable, KindSignatures #-}
module TreeSitter.Token
( Token(..)
) where

import GHC.Generics (Generic, Generic1)
import GHC.TypeLits (Symbol, Nat)

newtype Token (symName :: Symbol) (symVal :: Nat) a = Token { ann :: a }
  deriving (Eq, Foldable, Functor, Generic, Generic1, Ord, Show, Traversable)
