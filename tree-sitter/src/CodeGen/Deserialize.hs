{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
module CodeGen.Deserialize
( Datatype (..)
, Field (..)
, Required (..)
, Type (..)
, DatatypeName (..)
, Named (..)
, Multiple (..)
) where

import Data.Aeson as Aeson
import Data.Aeson.Types
import Data.Char
import GHC.Generics hiding (Constructor, Datatype)
import Data.Text (Text, unpack)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.HashMap.Strict as HM

-- Types to deserialize into:
data Datatype
  = SumType
  { datatypeName     :: DatatypeName
  , isName           :: Named
  , datatypeSubtypes :: [Type]
  }
  | ProductType
  { datatypeName   :: DatatypeName
  , isName         :: Named
  , datatypeFields :: NonEmpty Field
  }
  | LeafType
  { datatypeName :: DatatypeName
  , isName       :: Named
  }
  deriving (Eq, Ord, Show, Generic, ToJSON)

instance FromJSON Datatype where
  parseJSON = withObject "Datatype" $ \v -> do
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
parseKVPairs :: NonEmpty (Text, Value) -> Parser (NonEmpty Field)
parseKVPairs = traverse go
  where go :: (Text, Value) -> Parser Field
        go (t,v) = do
          v' <- parseJSON v
          pure $ v' { fieldName = Just (unpack t) }

data Field = Field
  { fieldRequired :: Required
  , fieldTypes    :: [Type]
  , fieldMultiple :: Multiple
  , fieldName     :: Maybe String
  }
  deriving (Eq, Ord, Show, Generic, ToJSON)

instance FromJSON Field where
  parseJSON = genericParseJSON customOptions

data Required = Optional | Required
  deriving (Eq, Ord, Show, Generic, ToJSON)

instance FromJSON Required where
  parseJSON = withBool "Required" (\p -> pure (if p then Required else Optional))

data Type = MkType
  { fieldType :: DatatypeName
  , isNamed :: Named
  }
  deriving (Eq, Ord, Show, Generic, ToJSON)

instance FromJSON Type where
  parseJSON = genericParseJSON customOptions

newtype DatatypeName = DatatypeName { getDatatypeName :: String }
  deriving (Eq, Ord, Show, Generic)
  deriving newtype (FromJSON, ToJSON)

data Named = Anonymous | Named
  deriving (Eq, Ord, Show, Generic, ToJSON)

instance FromJSON Named where
  parseJSON = withBool "Named" (\p -> pure (if p then Named else Anonymous))

data Multiple = Single | Multiple
  deriving (Eq, Ord, Show, Generic, ToJSON)

instance FromJSON Multiple where
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
