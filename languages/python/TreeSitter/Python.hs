{-# LANGUAGE DeriveAnyClass, DeriveGeneric, DuplicateRecordFields, TemplateHaskell #-}
module TreeSitter.Python where

import Language.Haskell.TH
import TreeSitter.Python.Internal
import TreeSitter.Language
import CodeGen.GenerateSyntax
import Control.Monad.IO.Class
import Data.Aeson
import Prelude hiding (Float, Integer, String)
import System.Directory
import System.FilePath.Posix
import Language.Haskell.TH.Syntax (loc_filename, location, runIO)

-- Regenerate template haskell code when these files change:
addDependentFileRelative "../vendor/tree-sitter-python/src/parser.c"

-- | Statically-known rules corresponding to symbols in the grammar.
mkSymbolDatatype (mkName "Grammar") tree_sitter_python

-- Auto-generate code from node-types.json
$(do
  currentFilename <- loc_filename <$> location
  pwd             <- runIO getCurrentDirectory
  let invocationRelativePath = takeDirectory (pwd </> currentFilename) </> "../vendor/tree-sitter-python/src/node-types.json"
  input <- runIO (eitherDecodeFileStrict' invocationRelativePath)
  either fail (traverse datatypeForConstructors) input)
