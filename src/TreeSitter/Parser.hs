module TreeSitter.Parser where

import Foreign
import Foreign.C
import TreeSitter.Language
import TreeSitter.Tree
import Data.IORef
import qualified Data.ByteString as Strict
import           Data.ByteString.Unsafe as Strict
import qualified Data.ByteString.Lazy as Lazy
import qualified Data.ByteString.Lazy.Internal as Lazy

newtype Parser = Parser ()
  deriving (Show, Eq)

type ReadCallback = Word32 -> Ptr Word32 -> IO CString
foreign import ccall "wrapper" mkReadCallback :: ReadCallback -> IO (FunPtr ReadCallback)

foreign import ccall safe "ts_parser_new" ts_parser_new :: IO (Ptr Parser)
foreign import ccall safe "ts_parser_halt_on_error" ts_parser_halt_on_error :: Ptr Parser -> CBool -> IO ()
foreign import ccall safe "ts_parser_parse_string" ts_parser_parse_string :: Ptr Parser -> Ptr Tree -> CString -> Int -> IO (Ptr Tree)
foreign import ccall safe "ts_parser_delete" ts_parser_delete :: Ptr Parser -> IO ()
foreign import ccall safe "ts_parser_set_language" ts_parser_set_language :: Ptr Parser -> Ptr Language -> IO ()
foreign import ccall safe "ts_parser_timeout_micros" ts_parser_timeout_micros :: Ptr Parser -> IO Word64
foreign import ccall safe "ts_parser_set_timeout_micros" ts_parser_set_timeout_micros :: Ptr Parser -> Word64 -> IO ()

foreign import ccall safe "src/bridge.c ts_parser_log_to_stderr" ts_parser_log_to_stderr :: Ptr Parser -> IO ()
foreign import ccall safe "src/bridge.c ts_parser_parse_with_callback" ts_parser_parse_with_callback
  :: Ptr Parser -> Ptr Tree -> FunPtr ReadCallback -> IO (Ptr Tree)

ts_parse_parse_lazily :: Ptr Parser -> Ptr Tree -> Lazy.ByteString -> IO (Ptr Tree)
ts_parse_parse_lazily parser old source = do
  marker <- newIORef source
  callback <- mkReadCallback $ \offset bytesReadPtr -> do
    wasRead <- peek bytesReadPtr
    print ("Callback recieved, offset ", offset, " bytes read ", wasRead)
    val <- readIORef marker
    case val of
      Lazy.Empty -> nullPtr <$ poke bytesReadPtr 0
      Lazy.Chunk chunk rest -> do
        poke bytesReadPtr (fromIntegral (Strict.length chunk))
        writeIORef marker rest
        Strict.unsafeUseAsCString chunk pure
  ts_parser_parse_with_callback parser old callback

