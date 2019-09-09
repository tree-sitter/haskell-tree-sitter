{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}

-- {-# LANGUAGE TypeOperators #-}
module TreeSitter.GenerateSyntax
( syntaxDatatype
, removeUnderscore
, initUpper
, astDeclarationsForLanguage
-- * Internal functions exposed for testing

) where

import Data.Char
import Language.Haskell.TH as TH
import Data.HashSet (HashSet)
import TreeSitter.Deserialize (Datatype (..), DatatypeName (..), Field (..), Children(..), Required (..), Type (..), Named (..), Multiple (..))
import Data.List.NonEmpty (NonEmpty (..))
import Data.Foldable
import Data.Text (Text)
import qualified Data.HashSet as HashSet
import qualified TreeSitter.Unmarshal as TS
import GHC.Generics hiding (Constructor, Datatype)
import Foreign.Ptr
import qualified TreeSitter.Language as TS
import Foreign.C.String
import Data.Proxy
import Data.Aeson hiding (String)
import System.Directory
import System.FilePath.Posix
import TreeSitter.Node
import TreeSitter.Symbol (escapeOperatorPunctuation)


-- Auto-generate Haskell datatypes from node-types.json
astDeclarationsForLanguage :: Ptr TS.Language -> FilePath -> Q [Dec]
astDeclarationsForLanguage language filePath = do
  _ <- TS.addDependentFileRelative filePath
  currentFilename <- loc_filename <$> location
  pwd             <- runIO getCurrentDirectory
  let invocationRelativePath = takeDirectory (pwd </> currentFilename) </> filePath
  input <- runIO (eitherDecodeFileStrict' invocationRelativePath)
  either fail (fmap (concat @[]) . traverse (syntaxDatatype language)) input


-- Auto-generate Haskell datatypes for sums, products and leaf types
syntaxDatatype :: Ptr TS.Language -> Datatype -> Q [Dec]
syntaxDatatype language datatype = do
  typeParameterName <- newName "a"
  case datatype of
    SumType (DatatypeName datatypeName) _ subtypes -> do
      cons <- traverse (constructorForSumChoice datatypeName typeParameterName) subtypes
      result <- symbolMatchingInstanceForSums language name subtypes typeParameterName
      pure $ generatedDatatype name cons typeParameterName:result
    ProductType (DatatypeName datatypeName) _ children fields -> do
      con <- ctorForProductType datatypeName typeParameterName children fields
      result <- symbolMatchingInstance language name datatypeName typeParameterName
      pure $ generatedDatatype name [con] typeParameterName:result
    LeafType (DatatypeName datatypeName) named -> do
      con <- ctorForLeafType named (DatatypeName datatypeName) typeParameterName
      result <- symbolMatchingInstance language name datatypeName typeParameterName
      pure $ case named of
        Anonymous -> generatedDatatype name [con] typeParameterName:result
        Named -> generatedDatatype name [con] typeParameterName:result
  where
    name = toName (datatypeNameStatus datatype) (getDatatypeName (TreeSitter.Deserialize.datatypeName datatype))
    deriveClause = [ DerivClause Nothing [ ConT ''TS.Unmarshal, ConT ''Eq, ConT ''Ord, ConT ''Show, ConT ''Generic ] ]
    generatedDatatype name cons typeParameterName = DataD [] name [PlainTV typeParameterName] Nothing cons deriveClause


-- | Create TH-generated SymbolMatching instances for sums, products, leaves
symbolMatchingInstance :: Ptr TS.Language -> Name -> String -> Name -> Q [Dec]
symbolMatchingInstance language name str typeParameterName = do
  tsSymbol <- runIO $ withCString str (pure . TS.ts_language_symbol_for_name language)
  let tsSymbolType = toEnum $ TS.ts_language_symbol_type language tsSymbol
  [d|instance TS.SymbolMatching $(appT (conT name) (varT typeParameterName)) where
      showFailure _ node = "Expected " <> $(litE (stringL (show name))) <> " but got " <> show (TS.fromTSSymbol (nodeSymbol node) :: $(conT (mkName "Grammar.Grammar")))
      symbolMatch _ node = TS.fromTSSymbol (nodeSymbol node) == $(conE (mkName $ "Grammar." <> TS.symbolToName tsSymbolType str))|]

symbolMatchingInstanceForSums ::  Ptr TS.Language -> Name -> [TreeSitter.Deserialize.Type] -> Name -> Q [Dec]
symbolMatchingInstanceForSums _ name subtypes typeParameterName =
  [d|instance TS.SymbolMatching $(appT (conT name) (varT typeParameterName)) where
      showFailure _ node = "Expected " <> $(litE (stringL (show (map extractn subtypes)))) <> " but got " <> show (TS.fromTSSymbol (nodeSymbol node) :: $(conT (mkName "Grammar.Grammar")))
      symbolMatch _ = $(foldr1 mkOr (perMkType `map` subtypes)) |]
  where perMkType (MkType (DatatypeName n) named) = [e|TS.symbolMatch (Proxy :: Proxy $(appT (conT (toName named n)) (varT typeParameterName))) |]
        mkOr lhs rhs = [e| (||) <$> $(lhs) <*> $(rhs) |]
        extractn (MkType (DatatypeName n) Named) = toCamelCase n
        extractn (MkType (DatatypeName n) Anonymous) = "Anonymous" <> toCamelCase n


-- | Append string with constructor name (ex., @IfStatementStatement IfStatement@)
constructorForSumChoice :: String -> Name -> TreeSitter.Deserialize.Type -> Q Con
constructorForSumChoice str typeParameterName (MkType (DatatypeName n) named) = normalC (toName named (n ++ str)) [child]
  where child = TH.bangType strictness (appT (conT (toName named n)) (varT typeParameterName))

-- | Build Q Constructor for product types (nodes with fields)
ctorForProductType :: String -> Name -> Maybe Children -> [(String, Field)] -> Q Con
ctorForProductType constructorName typeParameterName children fields = ctorForTypes constructorName lists where
  lists = annotation : fieldList ++ childList
  annotation = ("ann", varT typeParameterName)
  fieldList = map (fmap toType) fields
  childList = toList $ fmap toTypeChild children
  toType (MkField required fieldTypes mult) =
    let ftypes = fieldTypesToNestedEither fieldTypes typeParameterName
    in case (required, mult) of
      (Required, Multiple) -> appT (conT ''NonEmpty) ftypes
      (Required, Single) -> ftypes
      (Optional, Multiple) -> appT (conT ''[]) ftypes
      (Optional, Single) -> appT (conT ''Maybe) ftypes
  toTypeChild (MkChildren field) = ("extra_children", toType field)


-- | Build Q Constructor for leaf types (nodes with no fields or subtypes)
ctorForLeafType :: Named -> DatatypeName -> Name -> Q Con
ctorForLeafType named datatypeName typeParameterName =
  case (named, datatypeName) of
    (Anonymous, DatatypeName name) -> ctorForTypesA name [annotation, ("bytes", conT ''Text)]
    (Named, DatatypeName name) -> ctorForTypes name [annotation, ("bytes", conT ''Text)]
  where annotation = ("ann", varT typeParameterName) -- ann :: a


-- | Build Q Constructor for records
ctorForTypes :: String -> [(String, Q TH.Type)] -> Q Con
ctorForTypes constructorName types = recC (toName Named constructorName) recordFields where
  recordFields = map (uncurry toVarBangType) types
  toVarBangType str type' = TH.varBangType (mkName . addTickIfNecessary . removeUnderscore $ str) (TH.bangType strictness type')

-- | Build Q Constructor for records
ctorForTypesA :: String -> [(String, Q TH.Type)] -> Q Con
ctorForTypesA constructorName types = recC (toName Anonymous constructorName) recordFields where
  recordFields = map (uncurry toVarBangType) types
  toVarBangType str type' = TH.varBangType (mkName . addTickIfNecessary . removeUnderscore $ str) (TH.bangType strictness type')


-- | Convert field types to Q types
fieldTypesToNestedEither :: NonEmpty TreeSitter.Deserialize.Type -> Name -> Q TH.Type
fieldTypesToNestedEither xs typeParameterName = foldr1 combine $ fmap convertToQType xs
  where
    combine convertedQType = appT (appT (conT ''Either) convertedQType)
    convertToQType (MkType (DatatypeName n) named) = appT (conT (toName named n)) (varT typeParameterName)
    -- TODO: pull convertToQType out to top-level fn

-- | Create bang required to build records
strictness :: BangQ
strictness = TH.bang noSourceUnpackedness noSourceStrictness

-- | Convert snake_case string to CamelCase String
toCamelCase :: String -> String
toCamelCase = initUpper . escapeOperatorPunctuation . removeUnderscore

clashingNames :: HashSet String
clashingNames = HashSet.fromList ["type", "module", "data"]

addTickIfNecessary :: String -> String
addTickIfNecessary s
  | HashSet.member s clashingNames = s ++ "'"
  | otherwise                      = s

-- | Prepend "Anonymous" to named node when false, otherwise use regular toName
toName :: Named -> String -> Name
toName named str = mkName $ addTickIfNecessary $ case named of
  Anonymous -> "Anonymous" <> toCamelCase str
  Named -> toCamelCase str

-- Helper function to output camel cased data type names
initUpper :: String -> String
initUpper (c:cs) = toUpper c : cs
initUpper "" = ""

-- Helper function to remove underscores from output of data type names
removeUnderscore :: String -> String
removeUnderscore = foldr appender ""
  where appender :: Char -> String -> String
        appender '_' cs = initUpper cs
        appender c cs = c : cs
