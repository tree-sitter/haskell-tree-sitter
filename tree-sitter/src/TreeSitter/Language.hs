{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE CApiFFI #-}
module TreeSitter.Language
( module TreeSitter.Language
, module TreeSitter.Symbol
) where

import           Data.Ix (Ix)
import           Data.List (mapAccumL)
import qualified Data.Set as Set
import           Data.Traversable (for)
import           Data.Word
import           Foreign.C.String
-- import           Foreign.Ptr
import           Language.Haskell.TH
import           Language.Haskell.TH.Syntax
import           System.Directory
import           System.FilePath.Posix
import           TreeSitter.Symbol
import Foreign.C.ConstPtr (ConstPtr(..))

-- | A tree-sitter language.
--
--   This type is uninhabited and used only for type safety within 'Ptr' values.
data Language

foreign import capi unsafe "tree_sitter/api.h ts_language_symbol_count" ts_language_symbol_count :: ConstPtr Language -> IO Word32
foreign import capi unsafe "tree_sitter/api.h ts_language_symbol_name" ts_language_symbol_name :: ConstPtr Language -> TSSymbol -> IO CString
foreign import capi unsafe "tree_sitter/api.h ts_language_symbol_type" ts_language_symbol_type :: ConstPtr Language -> TSSymbol -> IO Int
foreign import capi unsafe "tree_sitter/api.h ts_language_symbol_for_name" ts_language_symbol_for_name :: ConstPtr Language -> CString -> Int -> Bool -> IO TSSymbol

-- | TemplateHaskell construction of a datatype for the referenced Language.
mkSymbolDatatype :: Name -> ConstPtr Language -> Q [Dec]
mkSymbolDatatype name language = do
  symbols <- renameDups . map ((,) . fst <*> uncurry symbolToName) . (++ [(Regular, "ParseError")]) <$> runIO (languageSymbols language)
  Module _ modName <- thisModule
  let mkMatch symbolType str = match (conP (Name (OccName str) (NameQ modName)) []) (normalB [e|symbolType|]) []
  datatype <- dataD (pure []) name [] Nothing (flip normalC [] . mkName . snd <$> symbols)
    [ derivClause Nothing (map conT [ ''Bounded, ''Enum, ''Eq, ''Ix, ''Ord, ''Show ]) ]
  symbolInstance <- [d|
    instance Symbol $(conT name) where
      symbolType = $(lamCaseE (uncurry mkMatch <$> symbols)) |]
  pure (datatype : symbolInstance)

renameDups :: [(a, String)] -> [(a, String)]
renameDups = snd . mapAccumL go mempty
  where go done (ty, name) = let name' = rename name in (Set.insert name' done, (ty, name'))
          where rename name | name `Set.member` done = rename (name ++ "'")
                            | otherwise              = name

-- https://stackoverflow.com/questions/16163948/how-do-i-use-templatehaskells-adddependentfile-on-a-file-relative-to-the-file-b
addDependentFileRelative :: FilePath -> Q [Dec]
addDependentFileRelative relativeFile = do
    currentFilename <- loc_filename <$> location
    pwd             <- runIO getCurrentDirectory

    let invocationRelativePath = takeDirectory (pwd </> currentFilename) </> relativeFile

    addDependentFile invocationRelativePath

    return []


languageSymbols :: ConstPtr Language -> IO [(SymbolType, String)]
languageSymbols language = ts_language_symbol_count language >>= \ count -> for [0..fromIntegral (pred count)] $ \ symbol -> do
  cname <- ts_language_symbol_name language symbol
  name <- peekCString cname
  ty <- toEnum <$> ts_language_symbol_type language symbol
  pure (ty, name)
