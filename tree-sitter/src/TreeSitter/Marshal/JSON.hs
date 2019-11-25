module TreeSitter.Marshal.JSON where

import Data.Aeson as Aeson

-- Typeclass to generically marshal ASTs into JSON
class MarshalJSON t where
  marshal :: t a -> Value
