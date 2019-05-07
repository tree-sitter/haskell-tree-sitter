module TreeSitter.Importing where

import Control.Exception as Exc
import Data.ByteString

import           Data.ByteString.Unsafe (unsafeUseAsCStringLen)
import           Foreign
import TreeSitter.Node as TS
import TreeSitter.Parser as TS
import TreeSitter.Tree as TS

data Expression
      = NumberExpression Number | IdentifierExpression Identifier
      deriving (Eq, Ord, Show)

data Number = Number
      deriving (Eq, Ord, Show)

data Identifier = Identifier
      deriving (Eq, Ord, Show)

importByteString :: (Importing t) => Ptr TS.Parser -> ByteString -> IO (Maybe t)
importByteString parser bytestring =
  unsafeUseAsCStringLen bytestring $ \ (source, len) -> alloca (\ rootPtr -> do
      let acquire =
            ts_parser_parse_string parser nullPtr source len

      let release t
            | t == nullPtr = pure ()
            | otherwise = ts_tree_delete t

      let go treePtr =
            if treePtr == nullPtr
              then pure Nothing
              else do
                ts_tree_root_node_p treePtr rootPtr
                node <- peek rootPtr
                Just <$> import' node
      Exc.bracket acquire release go)


class Importing type' where

  import' :: Node -> IO type'


-- ToAST takes Node -> IO (..value of DT)
-- IO t



-- splice will generate instances of this class
-- th-syntax will import TreeSitter.Importing
--
