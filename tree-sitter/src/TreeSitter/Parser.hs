module TreeSitter.Parser
( Parser
, withParser
, withParseTree
, ts_parser_new
, ts_parser_parse_string
, ts_parser_delete
, ts_parser_set_language
, ts_parser_timeout_micros
, ts_parser_set_timeout_micros
, ts_parser_log_to_stderr
) where

import Control.Exception as Exc
import Data.ByteString (ByteString)
import Data.ByteString.Unsafe (unsafeUseAsCStringLen)
import Foreign
import Foreign.C
import TreeSitter.Language
import TreeSitter.Tree

-- | A tree-sitter parser.
--
--   This type is uninhabited and used only for type safety within 'Ptr' values.
data Parser

withParser :: Ptr Language -> (Ptr Parser -> IO a) -> IO a
withParser language action = Exc.bracket
  ts_parser_new
  ts_parser_delete
  $ \ parser -> do
    _ <- ts_parser_set_language parser language
    action parser

withParseTree :: Ptr Parser -> ByteString -> (Ptr Tree -> IO a) -> IO a
withParseTree parser bytestring action =
  unsafeUseAsCStringLen bytestring $ \ (source, len) -> Exc.bracket
    (ts_parser_parse_string parser nullPtr source len)
    releaseTree
    action
  where releaseTree t
          | t == nullPtr = pure ()
          | otherwise    = ts_tree_delete t

foreign import ccall safe "ts_parser_new" ts_parser_new :: IO (Ptr Parser)
foreign import ccall safe "ts_parser_parse_string" ts_parser_parse_string :: Ptr Parser -> Ptr Tree -> CString -> Int -> IO (Ptr Tree)
foreign import ccall safe "ts_parser_delete" ts_parser_delete :: Ptr Parser -> IO ()
foreign import ccall safe "ts_parser_set_language" ts_parser_set_language :: Ptr Parser -> Ptr Language -> IO Bool
foreign import ccall safe "ts_parser_timeout_micros" ts_parser_timeout_micros :: Ptr Parser -> IO Word64
foreign import ccall safe "ts_parser_set_timeout_micros" ts_parser_set_timeout_micros :: Ptr Parser -> Word64 -> IO ()

foreign import ccall safe "src/bridge.c ts_parser_log_to_stderr" ts_parser_log_to_stderr :: Ptr Parser -> IO ()
