{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

-- {-# LANGUAGE TypeOperators #-}
module TH where

import Data.Aeson as Aeson
import Data.Char
import Language.Haskell.TH
import Language.Haskell.TH.Syntax as TH
import GHC.Generics hiding (Constructor, Datatype)
import Control.Monad.IO.Class
import Deserialize (MkDatatype (..), MkDatatypeName (..), MkField (..), MkRequired (..), MkType (..), MkNamed (..), MkMultiple (..))
import Data.List.NonEmpty (NonEmpty (..))
import Data.Foldable

-- Read JSON as input
input :: Q [MkDatatype]
input = liftIO (eitherDecodeFileStrict' "test/test.json") >>= either fail pure

-- Template Haskell functions that take the input types and auto-generate Haskell datatypes
datatypeForConstructors :: MkDatatype -> Q Dec
datatypeForConstructors (SumType (DatatypeName datatypeName) named subtypes) = do
  let name = toName' named datatypeName
  cons <- traverse (toSumCon datatypeName) subtypes
  pure $ DataD [] name [] Nothing cons [ DerivClause Nothing [ ConT ''Eq, ConT ''Ord, ConT ''Show ] ]
datatypeForConstructors (ProductType (DatatypeName datatypeName) named fields) = do
  let name = toName' named datatypeName
  con <- toConProduct datatypeName fields
  pure $ DataD [] name [] Nothing [con] [ DerivClause Nothing [ ConT ''Eq, ConT ''Ord, ConT ''Show ] ]
datatypeForConstructors (LeafType (DatatypeName datatypeName) named) = do
  let name = toName' named datatypeName
  con <- toConLeaf named (DatatypeName datatypeName)
  pure $ DataD [] name [] Nothing [con] [ DerivClause Nothing [ ConT ''Eq, ConT ''Ord, ConT ''Show ] ]

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
toConLeaf named (DatatypeName name) = pure (NormalC (toName' named name) [])

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
  pure (mkName name, Bang TH.NoSourceUnpackedness TH.NoSourceStrictness, ty')
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

-- | Convert snake_case string to CamelCase Name
toName :: String -> Name
toName = mkName . toCamelCase

-- | Prepend "Anonymous" to named node when false, otherwise use regular toName
toName' :: MkNamed -> String -> Name
toName' Named str = mkName $ toCamelCase str
toName' Anonymous str = mkName ("Anonymous" <> toCamelCase str)

-- Helper function to output camel cased data type names
initUpper :: String -> String
initUpper (c:cs) = toUpper c : cs
initUpper "" = ""

-- Helper function to remove underscores from output of data type names
-- rename to dromedary case?
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
toDescription '{' = "LBrace"
toDescription '}' = "RBrace"
toDescription '(' = "LParen"
toDescription ')' = "RParen"
toDescription '.' = "Dot"
toDescription ':' = "Colon"
toDescription ',' = "Comma"
toDescription '|' = "Pipe"
toDescription ';' = "Semicolon"
toDescription '*' = "Star"
toDescription '&' = "Ampersand"
toDescription '=' = "Equal"
toDescription '<' = "LAngle"
toDescription '>' = "RAngle"
toDescription '[' = "LBracket"
toDescription ']' = "RBracket"
toDescription '+' = "Plus"
toDescription '-' = "Minus"
toDescription '/' = "Slash"
toDescription '\\' = "Backslash"
toDescription '^' = "Caret"
toDescription '!' = "Bang"
toDescription '%' = "Percent"
toDescription '@' = "At"
toDescription '~' = "Tilde"
toDescription '?' = "Question"
toDescription '`' = "Backtick"
toDescription '#' = "Hash"
toDescription '$' = "Dollar"
toDescription '"' = "DQuote"
toDescription '\'' = "SQuote"
toDescription '\t' = "Tab"
toDescription '\n' = "LF"
toDescription '\r' = "CR"
toDescription control
  | isControl control = mapOperator (show control)
toDescription c = [c]
