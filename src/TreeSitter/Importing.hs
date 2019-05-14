{-# LANGUAGE DeriveFunctor, FlexibleContexts, GeneralizedNewtypeDeriving, ScopedTypeVariables, TypeApplications #-}
module TreeSitter.Importing where

import Control.Exception as Exc
import Data.ByteString

import           Data.ByteString.Unsafe (unsafeUseAsCStringLen)
import           Foreign.Marshal.Alloc
import           Foreign.Ptr
import           Foreign.Storable
import TreeSitter.Cursor as TS
import TreeSitter.Node as TS
import TreeSitter.Parser as TS
import TreeSitter.Tree as TS
import qualified Data.Text as Text
import Control.Effect
import Control.Effect.Reader
import Control.Monad.IO.Class
import Data.Text.Encoding
import qualified Data.ByteString as B
import Control.Applicative
import Control.Monad (void)

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
                withCursor (castPtr rootPtr) $ \ cursor ->
                  Just <$> runM (runReader cursor (runReader bytestring (import' cursor)))
      Exc.bracket acquire release go)

withCursor :: Ptr TSNode -> (Ptr Cursor -> IO a) -> IO a
withCursor rootPtr action = allocaBytes sizeOfCursor $ \ cursor -> Exc.bracket_
  (ts_tree_cursor_new_p rootPtr cursor)
  (ts_tree_cursor_delete cursor)
  (action cursor)


instance (Importing a, Importing b) => Importing (a,b) where
  import' cursor = push cursor $ do
    a <- import' @a cursor
    _ <- liftIO $ ts_tree_cursor_goto_next_sibling cursor
    b <- import' @b cursor
    pure (a, b)


instance Importing Text.Text where
  import' cursor = do
    node <- liftIO $ alloca $ \ tsNodePtr -> do
      ts_tree_cursor_current_node_p cursor tsNodePtr
      alloca $ \ nodePtr -> do
        ts_node_poke_p tsNodePtr nodePtr
        peek nodePtr
    bytestring <- ask
    let start = fromIntegral (nodeStartByte node)
        end = fromIntegral (nodeEndByte node)
    pure (decodeUtf8 (slice start end bytestring))


instance (Importing a, Importing b) => Importing (Either a b) where
  import' cursor = push cursor $
    Left <$> import' @a cursor <|> Right <$> import' @b cursor

push :: MonadIO m => Ptr Cursor -> m a -> m a
push cursor m = do
  _ <- liftIO $ ts_tree_cursor_goto_first_child cursor
  a <- m
  _ <- liftIO $ ts_tree_cursor_goto_parent cursor
  pure a

step :: (Carrier sig m, Member (Reader (Ptr Cursor)) sig, MonadIO m) => ReaderC (Ptr Cursor) m ()
step = void $ ask >>= liftIO . ts_tree_cursor_goto_next_sibling

push' :: (Carrier sig m, Member (Reader (Ptr Cursor)) sig, MonadIO m) => m a -> m a
push' m = do
  void $ ask >>= liftIO . ts_tree_cursor_goto_first_child
  a <- m
  a <$ (ask >>= liftIO . ts_tree_cursor_goto_parent)

peekNode :: (Carrier sig m, Member (Reader (Ptr Cursor)) sig, MonadIO m) => m Node
peekNode = do
  cursor <- ask
  liftIO $ alloca $ \ tsNodePtr -> do
    ts_tree_cursor_current_node_p cursor tsNodePtr
    alloca $ \ nodePtr -> do
      ts_node_poke_p tsNodePtr nodePtr
      peek nodePtr


-- | Return a 'ByteString' that contains a slice of the given 'Source'.
slice :: Int -> Int -> ByteString -> ByteString
slice start end = take . drop
  where drop = B.drop start
        take = B.take (end - start)

class Importing type' where

  import' :: (Alternative m, Carrier sig m, Member (Reader ByteString) sig, Member (Reader (Ptr Cursor)) sig, MonadIO m) => Ptr Cursor -> m type'

newtype MaybeC m a = MaybeC { runMaybeC :: m (Maybe a) }
  deriving (Functor)

instance Applicative m => Applicative (MaybeC m) where
  pure a = MaybeC (pure (Just a))
  liftA2 f (MaybeC a) (MaybeC b) = MaybeC $ liftA2 (liftA2 f) a b


-----------------
-- | Notes
-- ToAST takes Node -> IO (value of datatype)
-- splice will generate instances of this class
-- CodeGen will import TreeSitter.Importing (why?)
-- Signal backtrackable failure
