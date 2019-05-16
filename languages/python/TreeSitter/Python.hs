{-# LANGUAGE DeriveAnyClass, DeriveGeneric, DuplicateRecordFields, TemplateHaskell #-}
module TreeSitter.Python where

import qualified Data.ByteString as B
import qualified Control.Exception as Exc
import Foreign.Ptr
import qualified TreeSitter.Importing as TS
import TreeSitter.Language
import TreeSitter.Parser
import CodeGen.Deserialize
import CodeGen.GenerateSyntax
import Control.Monad.IO.Class
import Data.Aeson as Aeson
import Prelude hiding (Float, Integer, String)

foreign import ccall unsafe "vendor/tree-sitter-python/src/parser.c tree_sitter_python" tree_sitter_python :: Ptr Language

-- Auto-generate code from node-types.json
$(do
  input <- liftIO (eitherDecodeFileStrict' "./vendor/tree-sitter-python/src/node-types.json")
  either fail (traverse datatypeForConstructors) input)


parseByteString :: B.ByteString -> IO (Maybe Module)
parseByteString bytestring = Exc.bracket
  ts_parser_new
  ts_parser_delete
  $ \ parser -> TS.parseByteString parser bytestring
