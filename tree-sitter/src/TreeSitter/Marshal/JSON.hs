{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}

module TreeSitter.Marshal.JSON where

import Data.Aeson as Aeson
import GHC.Generics
import Data.Proxy
import Data.Text (Text)
import qualified Data.Text as Text
import GHC.TypeLits

-- Serialize unmarshaled ASTs into JSON representation.

-- AST nodes are expressed as products, sums, named or anonymous leaves, meaning we can generically iterate over them and pass the results to Aeson to be expressed as JSON objects.

-- Typeclass to generically marshal ASTs into JSON
class MarshalJSON t where
  marshal :: t a -> Value
  default marshal :: ( Generic1 t, GMarshalJSON (Rep1 t)) => t a -> Value
  marshal = gmarshal . from1

class GMarshalJSON f where
  gmarshal :: f a -> Value

-- Stores meta-data for datatypes
instance GMarshalJSON f => GMarshalJSON (M1 i c f) where
  gmarshal = gmarshal . unM1
