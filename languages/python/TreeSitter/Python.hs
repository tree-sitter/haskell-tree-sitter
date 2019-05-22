{-# LANGUAGE DeriveAnyClass, DeriveGeneric, DuplicateRecordFields, TemplateHaskell #-}
module TreeSitter.Python where

import Foreign.Ptr
import TreeSitter.Language
import CodeGen.GenerateSyntax
import Control.Monad.IO.Class
import Data.Aeson
import Prelude hiding (Float, Integer, String)
import System.Directory
import System.FilePath.Posix
import Language.Haskell.TH.Syntax (loc_filename, location, runIO)

foreign import ccall unsafe "vendor/tree-sitter-python/src/parser.c tree_sitter_python" tree_sitter_python :: Ptr Language

-- FIXME: Currently, the following says the file does not exist:
-- $(do
--   input <- liftIO (eitherDecodeFileStrict' "./vendor/tree-sitter-python/src/node-types.json")
--   either fail (traverse datatypeForConstructors) input)

-- Auto-generate code from node-types.json
$(do
  currentFilename <- loc_filename <$> location
  pwd             <- runIO getCurrentDirectory
  let invocationRelativePath = takeDirectory (pwd </> currentFilename) </> "../vendor/tree-sitter-python/src/node-types.json"
  input <- liftIO (eitherDecodeFileStrict' invocationRelativePath)
  either fail (traverse datatypeForConstructors) input)
