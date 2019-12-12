{-# LANGUAGE DeriveAnyClass, DeriveGeneric, LambdaCase, TemplateHaskell, TypeApplications #-}

module TreeSitter.GenerateSyntax
( syntaxDatatype
, astDeclarationsForLanguage
) where

import Language.Haskell.TH as TH
import Language.Haskell.TH.Syntax as TH
import TreeSitter.Deserialize (Datatype (..), DatatypeName (..), Field (..), Children(..), Required (..), Type (..), Named (..), Multiple (..))
import Data.List.NonEmpty (NonEmpty (..))
import Data.List
import Data.Foldable
import qualified Data.Set as Set
import Data.Text (Text)
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
import TreeSitter.Symbol (TSSymbol, toHaskellCamelCaseIdentifier, toHaskellPascalCaseIdentifier)

-- | Derive Haskell datatypes from a language and its @node-types.json@ file.
--
-- Datatypes will be generated according to the specification in the @node-types.json@ file, with anonymous leaf types defined as synonyms for the 'Token' datatype.
--
-- Any datatypes among the node types whose names are in the passed list will be skipped, allowing customization of the representation of parts of the tree. Note that this should be used sparingly, as it imposes extra maintenance burden, particularly when the grammar is changed. This may be used to e.g. parse literals into Haskell equivalents (e.g. parsing the textual contents of integer literals into 'Integer's), and may require defining 'TS.UnmarshalAnn' or 'TS.SymbolMatching' instances for (parts of) the custom datatypes, depending on where and how the datatype occurs in the generated tree, in addition to the usual 'Foldable', 'Functor', etc. instances provided for generated datatypes.
astDeclarationsForLanguage :: Ptr TS.Language -> FilePath -> [Name] -> Q [Dec]
astDeclarationsForLanguage language filePath excludedNames = do
  _ <- TS.addDependentFileRelative filePath
  currentFilename <- loc_filename <$> location
  pwd             <- runIO getCurrentDirectory
  let invocationRelativePath = takeDirectory (pwd </> currentFilename) </> filePath
  input <- runIO (eitherDecodeFileStrict' invocationRelativePath) >>= either fail pure
  allSymbols <- runIO (getAllSymbols language)
  concat @[] <$> traverse (syntaxDatatype language allSymbols) (filter included input) where
  included datatype = let name = toNameString (datatypeNameStatus datatype) (getDatatypeName (TreeSitter.Deserialize.datatypeName datatype)) in Set.notMember name excludes
  excludes = Set.fromList (map (\ (TH.Name (TH.OccName s) _) -> s) excludedNames)

-- Build a list of all symbols
getAllSymbols :: Ptr TS.Language -> IO [(String, Named)]
getAllSymbols language = do
  count <- TS.ts_language_symbol_count language
  mapM getSymbol [(0 :: TSSymbol) .. fromIntegral (pred count)]
  where
    getSymbol i = do
      cname <- TS.ts_language_symbol_name language i
      n <- peekCString cname
      t <- TS.ts_language_symbol_type language i
      let named = if t == 0 then Named else Anonymous
      pure (n, named)

-- Auto-generate Haskell datatypes for sums, products and leaf types
syntaxDatatype :: Ptr TS.Language -> [(String, Named)] -> Datatype -> Q [Dec]
syntaxDatatype language allSymbols datatype = do
  typeParameterName <- newName "a"
  case datatype of
    SumType (DatatypeName _) _ subtypes -> do
      types' <- fieldTypesToNestedSum subtypes
      con <- normalC name [TH.bangType strictness (pure types' `appT` varT typeParameterName)]
      pure [NewtypeD [] name [PlainTV typeParameterName] Nothing con [deriveGN, deriveStockClause, deriveAnyClassClause]]
    ProductType (DatatypeName datatypeName) named children fields -> do
      con <- ctorForProductType datatypeName typeParameterName children fields
      result <- symbolMatchingInstance allSymbols name named datatypeName
      pure $ generatedDatatype name [con] typeParameterName:result
      -- Anonymous leaf types are defined as synonyms for the `Token` datatype
    LeafType (DatatypeName datatypeName) Anonymous -> do
      tsSymbol <- runIO $ withCStringLen datatypeName (\(s, len) -> TS.ts_language_symbol_for_name language s len False)
      pure [ TySynD name [] (ConT ''Token `AppT` LitT (StrTyLit datatypeName) `AppT` LitT (NumTyLit (fromIntegral tsSymbol))) ]
    LeafType (DatatypeName datatypeName) Named -> do
      con <- ctorForLeafType (DatatypeName datatypeName) typeParameterName
      result <- symbolMatchingInstance allSymbols name Named datatypeName
      pure $ generatedDatatype name [con] typeParameterName:result
  where
    name = mkName $ toNameString (datatypeNameStatus datatype) (getDatatypeName (TreeSitter.Deserialize.datatypeName datatype))
    deriveStockClause = DerivClause (Just StockStrategy) [ ConT ''Eq, ConT ''Ord, ConT ''Show, ConT ''Generic, ConT ''Foldable, ConT ''Functor, ConT ''Traversable, ConT ''Generic1]
    deriveAnyClassClause = DerivClause (Just AnyclassStrategy) [ConT ''TS.Unmarshal]
    deriveGN = DerivClause (Just NewtypeStrategy) [ConT ''TS.SymbolMatching]
    generatedDatatype name cons typeParameterName = DataD [] name [PlainTV typeParameterName] Nothing cons [deriveStockClause, deriveAnyClassClause]


-- | Create TH-generated SymbolMatching instances for sums, products, leaves
symbolMatchingInstance :: [(String, Named)] -> Name -> Named -> String -> Q [Dec]
symbolMatchingInstance allSymbols name named str = do
  let tsSymbols = elemIndices (str, named) allSymbols
  let names = intercalate ", " $ fmap (debugPrefix . (!!) allSymbols) tsSymbols
  [d|instance TS.SymbolMatching $(conT name) where
      showFailure _ node = "expected " <> $(litE (stringL (show names))) <> " but got " <> show (debugPrefix (allSymbols !! fromIntegral (nodeSymbol node)))
      symbolMatch _ node = elem (nodeSymbol node) tsSymbols|]

-- | Prefix symbol names for debugging to disambiguate between Named and Anonymous nodes.
debugPrefix :: (String, Named) -> String
debugPrefix (name, Named)     = name
debugPrefix (name, Anonymous) = "_" <> name

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
  toVarBangType str type' = TH.varBangType (mkName . toHaskellCamelCaseIdentifier $ str) (TH.bangType strictness type')


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

-- | Prepend "Anonymous" to named node when false, otherwise use regular toName
toName :: Named -> String -> Name
toName named str = mkName (toNameString named str)

toNameString :: Named -> String -> String
toNameString named str = prefix named <> toHaskellPascalCaseIdentifier str
  where
    prefix Anonymous = "Anonymous"
    prefix Named     = ""
