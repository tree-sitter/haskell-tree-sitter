{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}

module TreeSitter.Marshal.JSON where

import Data.Aeson as Aeson
import GHC.Generics

-- Typeclass to generically marshal ASTs into JSON
class MarshalJSON t where
  marshal :: t a -> Value
  default marshal :: ( Generic1 t, GMarshalJSON (Rep1 t)) => t a -> Value
  marshal = gmarshal . from1
class GMarshalJSON f where
  gmarshal :: f a -> Value
