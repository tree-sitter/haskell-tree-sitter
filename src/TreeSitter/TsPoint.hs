{-# LANGUAGE DeriveGeneric, InterruptibleFFI #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}
module TreeSitter.TsPoint
  ( TSPoint(..)
  )
where

import           Foreign
import           GHC.Generics

import           TreeSitter.Struct


data TSPoint = TSPoint { pointRow :: !Word32, pointColumn :: !Word32 }
  deriving (Show, Eq, Generic)

instance Storable TSPoint where
  alignment _ = alignment (0 :: Int32)
  sizeOf _ = 8
  peek = evalStruct $ TSPoint <$> peekStruct
                              <*> peekStruct
  poke ptr (TSPoint r c) = flip evalStruct ptr $ do
    pokeStruct r
    pokeStruct c

