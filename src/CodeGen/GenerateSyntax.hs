{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -ddump-splices #-}

-- {-# LANGUAGE TypeOperators #-}
module CodeGen.GenerateSyntax
( datatypeForConstructors
, removeUnderscore
, initUpper
, mapOperator
) where

import Data.Char
import Language.Haskell.TH
import Data.HashSet (HashSet)
import Language.Haskell.TH.Syntax as TH
import CodeGen.Deserialize (MkDatatype (..), MkDatatypeName (..), MkField (..), MkRequired (..), MkType (..), MkNamed (..), MkMultiple (..))
import Data.List.NonEmpty (NonEmpty (..))
import Data.Foldable
import Data.Text (Text)
import qualified Data.HashSet as HashSet
import qualified TreeSitter.Importing as TS
import GHC.Generics hiding (Constructor, Datatype)
import Foreign.Ptr
import qualified TreeSitter.Language as TS
import Foreign.C.String

-- Auto-generate Haskell datatypes for sums, products and leaf types
datatypeForConstructors :: Ptr TS.Language -> MkDatatype -> Q [Dec]
datatypeForConstructors language (SumType (DatatypeName datatypeName) named subtypes) = do
  let name = toName' named datatypeName
  cons <- traverse (toSumCon datatypeName) subtypes
  result <- symbolMatchingInstance language name datatypeName
  pure $ DataD [] name [] Nothing cons [ DerivClause Nothing [ ConT ''TS.Building, ConT ''Eq, ConT ''Generic, ConT ''Ord, ConT ''Show ] ]:result
datatypeForConstructors language (ProductType (DatatypeName datatypeName) named fields) = do
  let name = toName' named datatypeName
  con <- toConProduct datatypeName fields
  result <- symbolMatchingInstance language name datatypeName
  pure $ DataD [] name [] Nothing [con] [ DerivClause Nothing [ ConT ''TS.Building, ConT ''Eq, ConT ''Ord, ConT ''Show, ConT ''Generic ] ]:result
datatypeForConstructors language (LeafType (DatatypeName datatypeName) Anonymous) = do
  let name = toName' Anonymous datatypeName
  con <- toConLeaf Anonymous (DatatypeName datatypeName)
  result <- symbolMatchingInstance language name datatypeName
  pure $ DataD [] name [] Nothing [con] [ DerivClause Nothing [ ConT ''TS.Building, ConT ''Eq, ConT ''Ord, ConT ''Show, ConT ''Generic ] ]:result
datatypeForConstructors language (LeafType (DatatypeName datatypeName) named) = do
  let name = toName' named datatypeName
  con <- toConLeaf named (DatatypeName datatypeName)
  result <- symbolMatchingInstance language name datatypeName
  pure $ NewtypeD [] name [] Nothing con [ DerivClause Nothing [ ConT ''TS.Building, ConT ''Eq, ConT ''Ord, ConT ''Show, ConT ''Generic ] ]:result

-- | Create TH-generated SymbolMatching instances for sums, products, leaves
symbolMatchingInstance :: Ptr TS.Language -> Name -> String -> Q [Dec]
symbolMatchingInstance language name str = do
  tsSymbol <- runIO $ withCString str (pure . TS.ts_language_symbol_for_name language)
  [d|instance TS.SymbolMatching $(conT name) where symbolMatch _ node = nodeSymbol node == $(litE (integerL (fromIntegral tsSymbol)))|]

-- get symbol datatype, which has constructors representing each production rule
-- use toEnum to have its constructors represented by enum types
-- then figure out which of those enums match the Word16s, to get an idea of whether or not we're producing the correct instances?


-- so instead of using `MkSymbolDatatype` we wanna do something similar,
-- but rather than doing it for the entirety of the language we just want to do it
-- for the production rules we get for that node given by `nodeSymbol`?
-- we want to pick the relevant constructor for the `Word16`
-- The symbol datatypes have an `Enum` instance so there’s `toEnum`/`fromEnum`

-- toAST :: forall grammar . (Bounded grammar, Enum grammar) => TS.Node -> IO (Base (AST [] grammar) TS.Node)
-- toAST node@TS.Node{..} = do
--   let count = fromIntegral nodeChildCount
--   children <- allocaArray count $ \ childNodesPtr -> do
--     _ <- with nodeTSNode (`TS.ts_node_copy_child_nodes` childNodesPtr)
--     peekArray count childNodesPtr
--   pure $! In (Node (toEnum (min (fromIntegral nodeSymbol) (fromEnum (maxBound :: grammar)))) (Location (nodeRange node) (nodeSpan node))) children


-- | Append string with constructor name (ex., @IfStatementStatement IfStatement@)
toSumCon :: String -> MkType -> Q Con
toSumCon str (MkType (DatatypeName n) named) = toConSum (n ++ str) [MkType (DatatypeName n) named]

-- | Build Q Constructor for sum types (nodes without fields, only subtypes)
toConSum :: String -> [MkType] -> Q Con
toConSum constructorName subtypes = NormalC (toName constructorName) <$> traverse toBangType subtypes

-- | Build Q Constructor for product types (nodes with fields)
toConProduct :: String -> NonEmpty (String, MkField) -> Q Con
toConProduct constructorName fields = RecC (toName constructorName) <$> fieldList
  where fieldList = toList <$> traverse (uncurry toVarBangType) fields

-- | Build Q Constructor for leaf types (nodes with no fields or subtypes)
toConLeaf :: MkNamed -> MkDatatypeName -> Q Con
toConLeaf Anonymous (DatatypeName name) = pure (NormalC (toName' Anonymous name) [])
toConLeaf named (DatatypeName name) = RecC (toName' named name) <$> leafRecords
  where leafRecords = pure <$> toLeafVarBangTypes

-- | Produce VarBangTypes required to construct records of leaf types
toLeafVarBangTypes :: Q VarBangType
toLeafVarBangTypes = do
  leafVarBangTypes <- conT ''Text
  pure (mkName "bytes", Bang TH.NoSourceUnpackedness TH.NoSourceStrictness, leafVarBangTypes)

-- | Construct toBangType for use in above toConSum
toBangType :: MkType -> Q BangType
toBangType (MkType (DatatypeName n) named) = do
  bangSubtypes <- conT (toName' named n)
  pure (Bang TH.NoSourceUnpackedness TH.NoSourceStrictness, bangSubtypes)


-- | For product types, examine the field's contents required for generating
--   Haskell code with records in the case of ProductTypes
toVarBangType :: String -> MkField -> Q VarBangType
toVarBangType name (MkField required fieldType multiplicity) = do
  ty' <- ty
  let newName = mkName . addTickIfNecessary . removeUnderscore $ name
  pure (newName, Bang TH.NoSourceUnpackedness TH.NoSourceStrictness, ty')
  where ty = case required of
          Optional -> [t|Maybe $(mult)|]
          Required -> mult
        mult = case multiplicity of
          Multiple -> [t|[$(toType fieldType)]|]
          Single   -> toType fieldType

-- | Convert field types to Q types
toType :: [MkType] -> Q Type
toType [] = fail "no types" -- FIXME: clarify this error message
toType xs = foldr1 combine $ map convertToQType xs
  where
    combine convertedQType = appT (appT (conT ''Either) convertedQType)
    convertToQType (MkType (DatatypeName n) named) = conT (toName' named n)

-- | Convert snake_case string to CamelCase String
toCamelCase :: String -> String
toCamelCase = initUpper . mapOperator . removeUnderscore

clashingNames :: HashSet String
clashingNames = HashSet.fromList ["type", "module", "data"]

addTickIfNecessary :: String -> String
addTickIfNecessary s
  | HashSet.member s clashingNames = s ++ "'"
  | otherwise                        = s

-- | Convert snake_case string to CamelCase Name
toName :: String -> Name
toName = mkName . addTickIfNecessary . toCamelCase

-- | Prepend "Anonymous" to named node when false, otherwise use regular toName
toName' :: MkNamed -> String -> Name
toName' Named str = mkName $ toCamelCase str
toName' Anonymous str = mkName ("Anonymous" <> toCamelCase str)

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

-- Helper function to map operators to valid Haskell identifier
mapOperator :: String -> String
mapOperator = concatMap toDescription

-- Helper function to map operator characters to strings
toDescription :: Char -> String
toDescription = \case
  '{'  -> "LBrace"
  '}'  -> "RBrace"
  '('  -> "LParen"
  ')'  -> "RParen"
  '.'  -> "Dot"
  ':'  -> "Colon"
  ','  -> "Comma"
  '|'  -> "Pipe"
  ';'  -> "Semicolon"
  '*'  -> "Star"
  '&'  -> "Ampersand"
  '='  -> "Equal"
  '<'  -> "LAngle"
  '>'  -> "RAngle"
  '['  -> "LBracket"
  ']'  -> "RBracket"
  '+'  -> "Plus"
  '-'  -> "Minus"
  '/'  -> "Slash"
  '\\' -> "Backslash"
  '^'  -> "Caret"
  '!'  -> "Bang"
  '%'  -> "Percent"
  '@'  -> "At"
  '~'  -> "Tilde"
  '?'  -> "Question"
  '`'  -> "Backtick"
  '#'  -> "Hash"
  '$'  -> "Dollar"
  '"'  -> "DQuote"
  '\'' -> "SQuote"
  '\t' -> "Tab"
  '\n' -> "LF"
  '\r' -> "CR"
  other
    | isControl other -> mapOperator (show other)
    | otherwise       -> [other]
