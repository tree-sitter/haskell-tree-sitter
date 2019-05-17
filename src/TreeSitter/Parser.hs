module TreeSitter.Parser
( Parser
, withParser
, ts_parser_new
, ts_parser_halt_on_error
, ts_parser_parse_string
, ts_parser_delete
, ts_parser_set_language
, ts_parser_timeout_micros
, ts_parser_set_timeout_micros
, ts_parser_log_to_stderr
) where

import Control.Exception as Exc
import Foreign
import Foreign.C
import TreeSitter.Language
import TreeSitter.Tree

data Parser = Parser
  deriving (Show)

withParser :: Ptr Language -> (Ptr Parser -> IO a) -> IO a
withParser language action = Exc.bracket
  ts_parser_new
  ts_parser_delete
  $ \ parser -> do
    ts_parser_set_language parser language
    action parser

foreign import ccall safe "ts_parser_new" ts_parser_new :: IO (Ptr Parser)
foreign import ccall safe "ts_parser_halt_on_error" ts_parser_halt_on_error :: Ptr Parser -> CBool -> IO ()
foreign import ccall safe "ts_parser_parse_string" ts_parser_parse_string :: Ptr Parser -> Ptr Tree -> CString -> Int -> IO (Ptr Tree)
foreign import ccall safe "ts_parser_delete" ts_parser_delete :: Ptr Parser -> IO ()
foreign import ccall safe "ts_parser_set_language" ts_parser_set_language :: Ptr Parser -> Ptr Language -> IO ()
foreign import ccall safe "ts_parser_timeout_micros" ts_parser_timeout_micros :: Ptr Parser -> IO Word64
foreign import ccall safe "ts_parser_set_timeout_micros" ts_parser_set_timeout_micros :: Ptr Parser -> Word64 -> IO ()

foreign import ccall safe "src/bridge.c ts_parser_log_to_stderr" ts_parser_log_to_stderr :: Ptr Parser -> IO ()
