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

data Bar a = Bar
  { ann :: a
   , guy :: Text
  } deriving (Eq, Show, Generic1)


-- Serialize unmarshaled ASTs into JSON representation.

-- AST nodes are expressed as products, sums, named or anonymous leaves, meaning we can generically iterate over them and pass the results to Aeson to be expressed as JSON objects.

-- Typeclass to generically marshal ASTs into JSON
class MarshalJSON t where
  marshal :: t a -> Value
  default marshal :: ( Generic1 t, GMarshalJSON (Rep1 t)) => t a -> Value
  marshal = gmarshal . from1

class GMarshalJSON f where
  gmarshal :: f a -> Value

instance GMarshalJSON Bar

-- Stores meta-data for datatypes
instance GMarshalJSON f => GMarshalJSON (M1 i c f) where
  gmarshal = gmarshal . unM1

-- Need to fold over S1 product types and pass the result to Aeson objects
instance GFields bod => GMarshalJSON (C1 (MetaCons ctorname x y) bod) where
  gmarshal = object . gfields [] . unM1

-- Implement the product case
instance (GFields f, GFields g) => GFields (f :*: g) where
  gfields acc (f :*: g) = gfields (gfields acc g) f

-- Define a new class to operate on product field types;
-- Takes an accumulator, a datatype, and returns a new accumulator value.
class GFields f where
  gfields :: ToJSON a => [(Text, Value)] -> f a -> [(Text, Value)]
