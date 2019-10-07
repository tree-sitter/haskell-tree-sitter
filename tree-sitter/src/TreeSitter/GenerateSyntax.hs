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
    SumType (DatatypeName _) _ subtypes -> do
      types' <- fieldTypesToNestedSum subtypes
      con <- normalC name [TH.bangType strictness (pure types' `appT` varT typeParameterName)]
      pure [NewtypeD [] name [PlainTV typeParameterName] Nothing con [deriveGN, deriveStockClause, deriveAnyClassClause]]
    ProductType (DatatypeName datatypeName) _ children fields -> do
      con <- ctorForProductType datatypeName typeParameterName children fields
      result <- symbolMatchingInstance language name datatypeName
      pure $ generatedDatatype name [con] typeParameterName:result
    LeafType (DatatypeName datatypeName) named -> do
      con <- ctorForLeafType named (DatatypeName datatypeName) typeParameterName
      result <- symbolMatchingInstance language name datatypeName
      pure $ case named of
        Anonymous -> NewtypeD [] name [PlainTV typeParameterName] Nothing con [deriveStockClause, deriveAnyClassClause]:result
        Named -> generatedDatatype name [con] typeParameterName:result
  where
    name = toName (datatypeNameStatus datatype) (getDatatypeName (TreeSitter.Deserialize.datatypeName datatype))
    deriveStockClause = DerivClause (Just StockStrategy) [ ConT ''Eq, ConT ''Ord, ConT ''Show, ConT ''Generic, ConT ''Foldable, ConT ''Functor, ConT ''Traversable, ConT ''Generic1]
    deriveAnyClassClause = DerivClause (Just AnyclassStrategy) [ConT ''TS.Unmarshal]
    deriveGN = DerivClause (Just NewtypeStrategy) [ConT ''TS.SymbolMatching]
    generatedDatatype name cons typeParameterName = DataD [] name [PlainTV typeParameterName] Nothing cons [deriveStockClause, deriveAnyClassClause]


-- | Create TH-generated SymbolMatching instances for sums, products, leaves
symbolMatchingInstance :: Ptr TS.Language -> Name -> String -> Q [Dec]
symbolMatchingInstance language name str = do
  tsSymbol <- runIO $ withCString str (TS.ts_language_symbol_for_name language)
  tsSymbolType <- toEnum <$> runIO (TS.ts_language_symbol_type language tsSymbol)
  [d|instance TS.SymbolMatching $(conT name) where
      showFailure _ node = "Expected " <> $(litE (stringL (show name))) <> " but got " <> show (TS.fromTSSymbol (nodeSymbol node) :: $(conT (mkName "Grammar.Grammar")))
      symbolMatch _ node = TS.fromTSSymbol (nodeSymbol node) == $(conE (mkName $ "Grammar." <> TS.symbolToName tsSymbolType str))|]


-- | Build Q Constructor for product types (nodes with fields)
ctorForProductType :: String -> Name -> Maybe Children -> [(String, Field)] -> Q Con
ctorForProductType constructorName typeParameterName children fields = ctorForTypes Named constructorName lists where
  lists = annotation : fieldList ++ childList
  annotation = ("ann", varT typeParameterName)
  fieldList = map (fmap toType) fields
  childList = toList $ fmap toTypeChild children
  toType (MkField required fieldTypes mult) =
    let ftypes = fieldTypesToNestedSum fieldTypes `appT` varT typeParameterName
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
    (Anonymous, DatatypeName name) -> ctorForTypes Anonymous name [annotation]
    (Named, DatatypeName name) -> ctorForTypes Named name [annotation, ("bytes", conT ''Text)]
  where annotation = ("ann", varT typeParameterName) -- ann :: a

-- | Build Q Constructor for records
ctorForTypes :: Named -> String -> [(String, Q TH.Type)] -> Q Con
ctorForTypes named constructorName types = recC (toName named constructorName) recordFields where
  recordFields = map (uncurry toVarBangType) types
  toVarBangType str type' = TH.varBangType (mkName . addTickIfNecessary . removeUnderscore $ str) (TH.bangType strictness type')


-- | Convert field types to Q types
fieldTypesToNestedSum :: NonEmpty TreeSitter.Deserialize.Type -> Q TH.Type
fieldTypesToNestedSum xs = foldr1 combine (fmap convertToQType xs)
  where
    combine lhs rhs = (conT ''(:+:) `appT` lhs) `appT` rhs
    convertToQType (MkType (DatatypeName n) named) = conT (toName named n)


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
