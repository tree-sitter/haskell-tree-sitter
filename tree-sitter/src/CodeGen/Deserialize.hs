{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
module CodeGen.Deserialize
( MkDatatype (..)
, MkField (..)
, MkRequired (..)
, MkType (..)
, MkDatatypeName (..)
, MkNamed (..)
, MkMultiple (..)
) where

import Data.Aeson as Aeson
import Data.Aeson.Types
import Data.Char
import GHC.Generics hiding (Constructor, Datatype)
import Data.Text (Text, unpack)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.HashMap.Strict as HM

-- Types to deserialize into:
data MkDatatype
  = SumType
  { datatypeName :: MkDatatypeName
  , isName :: MkNamed
  , datatypeSubtypes :: [MkType]
  }
  | ProductType
  { datatypeName   :: MkDatatypeName
  , isName         :: MkNamed
  , datatypeFields :: NonEmpty (String, MkField)
  }
  | LeafType
  { datatypeName :: MkDatatypeName
  , isName       :: MkNamed
  }
  deriving (Eq, Ord, Show, Generic, ToJSON)

instance FromJSON MkDatatype where
  parseJSON = withObject "MkDatatype" $ \v -> do
    type' <- v .: "type"
    named <- v .: "named"
    subtypes <- v .:? "subtypes"
    case subtypes of
      Nothing -> do
        fields <- v .:? "fields"
        -- If fields are present, map to product type; otherwise map to NonEmpty leaf type
        case fmap HM.toList fields of
          Just (field:fields) -> ProductType type' named <$> parseKVPairs (field :| fields)
          Just [] -> pure (LeafType type' named)
          _ -> pure (LeafType type' named)
      Just subtypes   -> pure (SumType type' named subtypes)

-- | Transforms list of key-value pairs to a Parser
parseKVPairs :: NonEmpty (Text, Value) -> Parser (NonEmpty (String, MkField))
parseKVPairs = traverse go
  where go :: (Text, Value) -> Parser (String, MkField)
        go (t,v) = do
          v' <- parseJSON v
          pure (unpack t, v')

data MkField = MkField
  { fieldRequired :: MkRequired
  , fieldTypes     :: [MkType]
  , fieldMultiple :: MkMultiple
  }
  deriving (Eq, Ord, Show, Generic, ToJSON)

instance FromJSON MkField where
  parseJSON = genericParseJSON customOptions

data MkRequired = Optional | Required
  deriving (Eq, Ord, Show, Generic, ToJSON)

instance FromJSON MkRequired where
  parseJSON = withBool "Required" (\p -> pure (if p then Required else Optional))

data MkType = MkType
  { fieldType :: MkDatatypeName
  , isNamed :: MkNamed
  }
  deriving (Eq, Ord, Show, Generic, ToJSON)

instance FromJSON MkType where
  parseJSON = genericParseJSON customOptions

newtype MkDatatypeName = DatatypeName { getDatatypeName :: String }
  deriving (Eq, Ord, Show, Generic)
  deriving newtype (FromJSON, ToJSON)

data MkNamed = Anonymous | Named
  deriving (Eq, Ord, Show, Generic, ToJSON)

instance FromJSON MkNamed where
  parseJSON = withBool "Named" (\p -> pure (if p then Named else Anonymous))

data MkMultiple = Single | Multiple
  deriving (Eq, Ord, Show, Generic, ToJSON)

instance FromJSON MkMultiple where
  parseJSON = withBool "Multiple" (\p -> pure (if p then Multiple else Single))

customOptions :: Aeson.Options
customOptions = Aeson.defaultOptions
  {
    fieldLabelModifier = initLower . dropPrefix
  , constructorTagModifier = initLower
  }

dropPrefix :: String -> String
dropPrefix = Prelude.dropWhile isLower

initLower :: String -> String
initLower (c:cs) = toLower c : cs
initLower "" = ""
