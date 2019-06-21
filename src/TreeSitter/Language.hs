{-# LANGUAGE TemplateHaskell #-}
module TreeSitter.Language where

import Data.Ix (Ix)
import Data.Traversable (for)
import Data.Word
import Foreign.C.String
import Foreign.Ptr
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import System.Directory
import System.FilePath.Posix
import TreeSitter.Symbol

newtype Language = Language ()
  deriving (Show, Eq)

foreign import ccall unsafe "ts_language_symbol_count" ts_language_symbol_count :: Ptr Language -> Word32
foreign import ccall unsafe "ts_language_symbol_name" ts_language_symbol_name :: Ptr Language -> TSSymbol -> CString
foreign import ccall unsafe "ts_language_symbol_type" ts_language_symbol_type :: Ptr Language -> TSSymbol -> Int

-- | TemplateHaskell construction of a datatype for the referenced Language.
mkSymbolDatatype :: Name -> Ptr Language -> Q [Dec]
mkSymbolDatatype name language = do
  symbols <- (++ [(Regular, "ParseError")]) <$> runIO (languageSymbols language)
  let namedSymbols = renameDups [] $ ((,) . fst <*> uncurry symbolToName) <$> symbols

  Module _ modName <- thisModule
  pure
    [ DataD [] name [] Nothing (flip NormalC [] . mkName . snd <$> namedSymbols) [ DerivClause Nothing [ ConT ''Show, ConT ''Enum, ConT ''Eq, ConT ''Ord, ConT ''Bounded, ConT ''Ix ] ]
    , InstanceD Nothing [] (AppT (ConT ''Symbol) (ConT name)) [ FunD 'symbolType (uncurry (clause modName) <$> namedSymbols) ] ]
  where clause modName symbolType str = Clause [ ConP (Name (OccName str) (NameQ modName)) [] ] (NormalB (ConE (promote symbolType))) []
        promote Regular = 'Regular
        promote Anonymous = 'Anonymous
        promote Auxiliary = 'Auxiliary
        renameDups done [] = reverse done
        renameDups done ((ty, name):queue) = if elem name (snd <$> done)
                                      then renameDups done ((ty, name ++ "'") : queue)
                                      else renameDups ((ty, name) : done) queue

-- https://stackoverflow.com/questions/16163948/how-do-i-use-templatehaskells-adddependentfile-on-a-file-relative-to-the-file-b
addDependentFileRelative :: FilePath -> Q [Dec]
addDependentFileRelative relativeFile = do
    currentFilename <- loc_filename <$> location
    pwd             <- runIO getCurrentDirectory

    let invocationRelativePath = takeDirectory (pwd </> currentFilename) </> relativeFile

    addDependentFile invocationRelativePath

    return []


languageSymbols :: Ptr Language -> IO [(SymbolType, String)]
languageSymbols language = for [0..fromIntegral (pred count)] $ \ symbol -> do
  name <- peekCString (ts_language_symbol_name language symbol)
  pure (toEnum (ts_language_symbol_type language symbol), name)
  where count = ts_language_symbol_count language
