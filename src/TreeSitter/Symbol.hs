{-# LANGUAGE ScopedTypeVariables #-}
module TreeSitter.Symbol where

import Data.Word
import TreeSitter.Language

toSymbol :: forall symbol . Symbol symbol => Word16 -> symbol
toSymbol symbol = toEnum (min (fromIntegral symbol) (fromEnum (maxBound :: symbol)))
