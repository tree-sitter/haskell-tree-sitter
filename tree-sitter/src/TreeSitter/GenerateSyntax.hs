{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}

-- {-# LANGUAGE TypeOperators #-}
module TreeSitter.GenerateSyntax
( syntaxDatatype
, astDeclarationsForLanguage
) where

import Language.Haskell.TH as TH
import Language.Haskell.TH.Syntax as TH
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
import TreeSitter.Token
import TreeSitter.Strings
import TreeSitter.Symbol (escapeOperatorPunctuation)


-- | Derive Haskell datatypes from a language and its @node-types.json@ file.
--
-- Datatypes will be generated according to the specification in the @node-types.json@ file, with anonymous leaf types defined as synonyms for the 'Token' datatype.
--
-- Any datatypes among the node types which have already been defined in the module where the splice is run will be skipped, allowing customization of the representation of parts of the tree. Note that this should be used sparingly, as it imposes extra maintenance burden, particularly when the grammar is changed. This may be used to e.g. parse literals into Haskell equivalents (e.g. parsing the textual contents of integer literals into 'Integer's), and may require defining 'TS.UnmarshalAnn' or 'TS.SymbolMatching' instances for (parts of) the custom datatypes, depending on where and how the datatype occurs in the generated tree, in addition to the usual 'Foldable', 'Functor', etc. instances provided for generated datatypes.
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
syntaxDatatype language datatype = skipDefined $ do
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
      -- Anonymous leaf types are defined as synonyms for the `Token` datatype
    LeafType (DatatypeName datatypeName) Anonymous -> do
      tsSymbol <- runIO $ withCStringLen datatypeName (\(s, len) -> TS.ts_language_symbol_for_name language s len False)
      pure [ TySynD name [] (ConT ''Token `AppT` LitT (StrTyLit datatypeName) `AppT` LitT (NumTyLit (fromIntegral tsSymbol))) ]
    LeafType (DatatypeName datatypeName) Named -> do
      con <- ctorForLeafType (DatatypeName datatypeName) typeParameterName
      result <- symbolMatchingInstance language name datatypeName
      pure $ generatedDatatype name [con] typeParameterName:result
  where
    -- Skip generating datatypes that have already been defined (overridden) in the module where the splice is running.
    skipDefined m = do
      isLocal <- lookupTypeName nameStr >>= maybe (pure False) isLocalName
      if isLocal then pure [] else m
    name = mkName nameStr
    nameStr = toNameString (datatypeNameStatus datatype) (getDatatypeName (TreeSitter.Deserialize.datatypeName datatype))
    deriveStockClause = DerivClause (Just StockStrategy) [ ConT ''Eq, ConT ''Ord, ConT ''Show, ConT ''Generic, ConT ''Foldable, ConT ''Functor, ConT ''Traversable, ConT ''Generic1]
    deriveAnyClassClause = DerivClause (Just AnyclassStrategy) [ConT ''TS.Unmarshal]
    deriveGN = DerivClause (Just NewtypeStrategy) [ConT ''TS.SymbolMatching]
    generatedDatatype name cons typeParameterName = DataD [] name [PlainTV typeParameterName] Nothing cons [deriveStockClause, deriveAnyClassClause]


-- | Create TH-generated SymbolMatching instances for sums, products, leaves
symbolMatchingInstance :: Ptr TS.Language -> Name -> String -> Q [Dec]
symbolMatchingInstance language name str = do
  tsSymbol <- runIO $ withCStringLen str (\(s, len) -> TS.ts_language_symbol_for_name language s len True)
  tsSymbolType <- toEnum <$> runIO (TS.ts_language_symbol_type language tsSymbol)
  [d|instance TS.SymbolMatching $(conT name) where
      showFailure _ node = "Expected " <> $(litE (stringL (TS.symbolToName tsSymbolType str))) <> " but got " <> show (TS.fromTSSymbol (nodeSymbol node) :: $(conT (mkName "Grammar.Grammar")))
      symbolMatch _ node = TS.fromTSSymbol (nodeSymbol node) == $(conE (mkName $ "Grammar." <> TS.symbolToName tsSymbolType str))|]

-- | Build Q Constructor for product types (nodes with fields)
ctorForProductType :: String -> Name -> Maybe Children -> [(String, Field)] -> Q Con
ctorForProductType constructorName typeParameterName children fields = ctorForTypes constructorName lists where
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
ctorForLeafType :: DatatypeName -> Name -> Q Con
ctorForLeafType (DatatypeName name) typeParameterName = ctorForTypes name
  [ ("ann",  varT typeParameterName) -- ann :: a
  , ("text", conT ''Text)            -- text :: Text
  ]

-- | Build Q Constructor for records
ctorForTypes :: String -> [(String, Q TH.Type)] -> Q Con
ctorForTypes constructorName types = recC (toName Named constructorName) recordFields where
  recordFields = map (uncurry toVarBangType) types
  toVarBangType str type' = TH.varBangType (mkName . addTickIfNecessary . camelCase $ str) (TH.bangType strictness type')


-- | Convert field types to Q types
fieldTypesToNestedSum :: NonEmpty TreeSitter.Deserialize.Type -> Q TH.Type
fieldTypesToNestedSum xs = go (toList xs)
  where
    combine lhs rhs = (conT ''(:+:) `appT` lhs) `appT` rhs -- (((((a :+: b) :+: c) :+: d)) :+: e)   ((a :+: b) :+: (c :+: d))
    convertToQType (MkType (DatatypeName n) named) = conT (toName named n)
    go [x] = convertToQType x
    go xs = let (l,r) = splitAt (length xs `div` 2) xs in (combine (go l) (go r))


-- | Create bang required to build records
strictness :: BangQ
strictness = TH.bang noSourceUnpackedness noSourceStrictness

clashingNames :: HashSet String
clashingNames = HashSet.fromList ["type", "module", "data"]

addTickIfNecessary :: String -> String
addTickIfNecessary s
  | HashSet.member s clashingNames = s ++ "'"
  | otherwise                      = s

-- | Prepend "Anonymous" to named node when false, otherwise use regular toName
toName :: Named -> String -> Name
toName named str = mkName (toNameString named str)

toNameString :: Named -> String -> String
toNameString named str = addTickIfNecessary $ case named of
  Anonymous -> "Anonymous" <> toEscapedPascalCase str
  Named -> toEscapedPascalCase str

-- | Get the 'Module', if any, for a given 'Name'.
moduleForName :: Name -> Maybe Module
moduleForName n = Module . PkgName <$> namePackage n <*> (ModName <$> nameModule n)

-- | Test whether the name is defined in the module where the splice is executed.
isLocalName :: Name -> Q Bool
isLocalName n = (moduleForName n ==) . Just <$> thisModule

-- | Replace operators and convert snake_case String to PascalCase
toEscapedPascalCase :: String -> String
toEscapedPascalCase = capitalize . escapeOperatorPunctuation . camelCase
