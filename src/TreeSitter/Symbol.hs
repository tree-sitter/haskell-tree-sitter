{-# LANGUAGE ScopedTypeVariables #-}
module TreeSitter.Symbol where

import Data.Ix
import Data.Word

type TSSymbol = Word16

toSymbol :: forall symbol . Symbol symbol => TSSymbol -> symbol
toSymbol symbol = toEnum (min (fromIntegral symbol) (fromEnum (maxBound :: symbol)))


data SymbolType = Regular | Anonymous | Auxiliary
  deriving (Enum, Eq, Ord, Show)

class (Bounded s, Enum s, Ix s, Ord s, Show s) => Symbol s where
  symbolType :: s -> SymbolType
