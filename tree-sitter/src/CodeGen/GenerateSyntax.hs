{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}

-- {-# LANGUAGE TypeOperators #-}
module CodeGen.GenerateSyntax
( syntaxDatatype
, removeUnderscore
, initUpper
, astDeclarationsForLanguage
-- * Internal functions exposed for testing

) where

import Data.Char
import Language.Haskell.TH as TH
import Data.HashSet (HashSet)
import Language.Haskell.TH.Syntax as TH
import CodeGen.Deserialize (Datatype (..), DatatypeName (..), Field (..), Required (..), Type (..), Named (..))
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
syntaxDatatype language datatype = case datatype of
  SumType (DatatypeName datatypeName) _ subtypes -> do
    cons <- traverse (toSumCon datatypeName) subtypes
    result <- symbolMatchingInstanceForSums language name subtypes
    pure $ generatedDatatype name cons:result
  ProductType (DatatypeName datatypeName) _ fields -> do
    con <- toConProduct datatypeName fields
    result <- symbolMatchingInstance language name datatypeName
    pure $ generatedDatatype name [con]:result
  LeafType (DatatypeName datatypeName) named -> do
    con <- ctorForLeafType named (DatatypeName datatypeName)
    result <- symbolMatchingInstance language name datatypeName
    pure $ case named of
      Anonymous -> generatedDatatype name [con]:result
      Named -> NewtypeD [] name [] Nothing con deriveClause:result
  where
    name = toName (datatypeNameStatus datatype) (getDatatypeName (CodeGen.Deserialize.datatypeName datatype))
    deriveClause = [ DerivClause Nothing [ ConT ''TS.Unmarshal, ConT ''Eq, ConT ''Ord, ConT ''Show, ConT ''Generic ] ]
    generatedDatatype name cons = DataD [] name [] Nothing cons deriveClause


-- | Create TH-generated SymbolMatching instances for sums, products, leaves
symbolMatchingInstance :: Ptr TS.Language -> Name -> String -> Q [Dec]
symbolMatchingInstance language name str = do
  tsSymbol <- runIO $ withCString str (pure . TS.ts_language_symbol_for_name language)
  let tsSymbolType = toEnum $ TS.ts_language_symbol_type language tsSymbol
  [d|instance TS.SymbolMatching $(conT name) where
      showFailure _ node = "Expected " <> $(litE (stringL (show name))) <> " but got " <> show (TS.fromTSSymbol (nodeSymbol node) :: $(conT (mkName "Grammar.Grammar")))
      symbolMatch _ node = TS.fromTSSymbol (nodeSymbol node) == $(conE (mkName $ "Grammar." <> TS.symbolToName tsSymbolType str))|]

symbolMatchingInstanceForSums ::  Ptr TS.Language -> Name -> [CodeGen.Deserialize.Type] -> Q [Dec]
symbolMatchingInstanceForSums _ name subtypes =
  [d|instance TS.SymbolMatching $(conT name) where
      showFailure _ node = "Expected " <> $(litE (stringL (show (map extractn subtypes)))) <> " but got " <> show (TS.fromTSSymbol (nodeSymbol node) :: $(conT (mkName "Grammar.Grammar")))
      symbolMatch _ = $(foldr1 mkOr (perMkType `map` subtypes)) |]
  where perMkType (MkType (DatatypeName n) named) = [e|TS.symbolMatch (Proxy :: Proxy $(conT (toName named n))) |]
        mkOr lhs rhs = [e| (||) <$> $(lhs) <*> $(rhs) |]
        extractn (MkType (DatatypeName n) Named) = toCamelCase n
        extractn (MkType (DatatypeName n) Anonymous) = "Anonymous" <> toCamelCase n


-- | Append string with constructor name (ex., @IfStatementStatement IfStatement@)
toSumCon :: String -> CodeGen.Deserialize.Type -> Q Con
toSumCon str (MkType (DatatypeName n) named) = NormalC (toName named (n ++ str)) <$> traverse toBangType [MkType (DatatypeName n) named]

-- | Build Q Constructor for product types (nodes with fields)
toConProduct :: String -> NonEmpty (String, Field) -> Q Con
toConProduct constructorName fields = RecC (toName Named constructorName) <$> fieldList
  where fieldList = toList <$> traverse (uncurry toVarBangType) fields

-- | Build Q Constructor for leaf types (nodes with no fields or subtypes)
ctorForLeafType :: Named -> DatatypeName -> Q Con
ctorForLeafType Anonymous (DatatypeName name) = normalC (toName Anonymous name) []
ctorForLeafType Named (DatatypeName name) = recC (toName Named name) [leafBytes] where
  leafBytes = TH.varBangType (mkName "bytes") textValue
  textValue = TH.bangType (TH.bang noSourceUnpackedness noSourceStrictness) (conT ''Text)

-- | Construct toBangType for use in above toConSum
toBangType :: CodeGen.Deserialize.Type -> Q BangType
toBangType (MkType (DatatypeName n) named) = do
  bangSubtypes <- conT (toName named n)
  pure (Bang TH.NoSourceUnpackedness TH.NoSourceStrictness, bangSubtypes)

-- | For product types, examine the field's contents required for generating
--   Haskell code with records in the case of ProductTypes
toVarBangType :: String -> Field -> Q VarBangType
toVarBangType name (MkField required fieldTypes _) = do
  ty' <- ty
  let newName = mkName . addTickIfNecessary . removeUnderscore $ name
  pure (newName, Bang TH.NoSourceUnpackedness TH.NoSourceStrictness, ty')
  where ty = case required of
          Optional -> [t|Maybe $(mult)|]
          Required -> mult
        mult = fieldTypesToNestedEither fieldTypes

-- | Convert field types to Q types
fieldTypesToNestedEither :: NonEmpty CodeGen.Deserialize.Type -> Q TH.Type
fieldTypesToNestedEither xs = foldr1 combine $ fmap convertToQType xs
  where
    combine convertedQType = appT (appT (conT ''Either) convertedQType)
    convertToQType (MkType (DatatypeName n) named) = conT (toName named n)

-- | Convert snake_case string to CamelCase String
toCamelCase :: String -> String
toCamelCase = initUpper . escapeOperatorPunctuation . removeUnderscore

clashingNames :: HashSet String
clashingNames = HashSet.fromList ["type", "module", "data"]

addTickIfNecessary :: String -> String
addTickIfNecessary s
  | HashSet.member s clashingNames = s ++ "'"
  | otherwise                        = s

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
