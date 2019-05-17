{-# LANGUAGE DefaultSignatures, DeriveFunctor, FlexibleContexts, FlexibleInstances, GeneralizedNewtypeDeriving,
             ScopedTypeVariables, TypeApplications, TypeOperators #-}
module TreeSitter.Importing
( parseByteString
, FieldName(..)
, Building(..)
) where

import           Control.Applicative
import           Control.Effect hiding ((:+:))
import           Control.Effect.Reader
import           Control.Exception as Exc
import           Control.Monad (void)
import           Control.Monad.IO.Class
import           Data.ByteString (ByteString)
import qualified Data.ByteString as B
import           Data.ByteString.Unsafe (unsafeUseAsCStringLen)
import qualified Data.Map as Map
import qualified Data.Text as Text
import           Data.Text.Encoding
import           Foreign.C.String
import           Foreign.Marshal.Alloc
import           Foreign.Marshal.Utils
import           Foreign.Ptr
import           Foreign.Storable
import           GHC.Generics
import           TreeSitter.Cursor as TS
import           TreeSitter.Node as TS
import           TreeSitter.Parser as TS
import           TreeSitter.Tree as TS

step :: (Carrier sig m, Member (Reader (Ptr Cursor)) sig, MonadIO m) => m ()
step = void $ ask >>= liftIO . ts_tree_cursor_goto_next_sibling

push :: (Carrier sig m, Member (Reader (Ptr Cursor)) sig, MonadIO m) => m a -> m a
push m = do
  void $ ask >>= liftIO . ts_tree_cursor_goto_first_child
  a <- m
  a <$ (ask >>= liftIO . ts_tree_cursor_goto_parent)

goto :: (Carrier sig m, Member (Reader (Ptr Cursor)) sig, MonadIO m) => TSNode -> m ()
goto node = do
  cursor <- ask
  liftIO (with node (ts_tree_cursor_reset_p cursor))

peekNode :: (Carrier sig m, Member (Reader (Ptr Cursor)) sig, MonadIO m) => m (Maybe Node)
peekNode = do
  cursor <- ask
  liftIO $ alloca $ \ tsNodePtr -> do
    isValid <- ts_tree_cursor_current_node_p cursor tsNodePtr
    if isValid then do
      node <- alloca $ \ nodePtr -> do
        ts_node_poke_p tsNodePtr nodePtr
        peek nodePtr
      pure (Just node)
    else
      pure Nothing

peekFieldName :: (Carrier sig m, Member (Reader (Ptr Cursor)) sig, MonadIO m) => m (Maybe FieldName)
peekFieldName = do
  cursor <- ask
  fieldName <- liftIO $ ts_tree_cursor_current_field_name cursor
  if fieldName == nullPtr then
    pure Nothing
  else
    Just . FieldName <$> liftIO (peekCString fieldName)

getFields :: (Carrier sig m, Member (Reader (Ptr Cursor)) sig, MonadIO m) => m (Map.Map FieldName Node)
getFields = go Map.empty
  where go fs = do
          node <- peekNode
          case node of
            Just node' -> do
              fieldName <- peekFieldName
              case fieldName of
                Just fieldName' -> do
                  step
                  go (Map.insert fieldName' node' fs)
                _ -> pure fs
            _ -> step *> go fs

-- | Return a 'ByteString' that contains a slice of the given 'Source'.
slice :: Int -> Int -> ByteString -> ByteString
slice start end = take . drop
  where drop = B.drop start
        take = B.take (end - start)


newtype MaybeC m a = MaybeC { runMaybeC :: m (Maybe a) }
  deriving (Functor)

instance Applicative m => Applicative (MaybeC m) where
  pure a = MaybeC (pure (Just a))
  liftA2 f (MaybeC a) (MaybeC b) = MaybeC $ liftA2 (liftA2 f) a b

instance Applicative m => Alternative (MaybeC m) where
  empty = MaybeC (pure Nothing)
  MaybeC a <|> MaybeC b = MaybeC (liftA2 (<|>) a b)

instance Monad m => Monad (MaybeC m) where
  MaybeC a >>= f = MaybeC $ do
    a' <- a
    case a' of
      Nothing -> pure Nothing
      Just a  -> runMaybeC $ f a

instance MonadIO m => MonadIO (MaybeC m) where
  liftIO = MaybeC . fmap Just . liftIO


----

parseByteString :: Building t => Ptr TS.Parser -> ByteString -> IO (Maybe t)
parseByteString parser bytestring =
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
                  runMaybeC (runM (runReader cursor (runReader bytestring buildNode)))
      Exc.bracket acquire release go)

withCursor :: Ptr TSNode -> (Ptr Cursor -> IO a) -> IO a
withCursor rootPtr action = allocaBytes sizeOfCursor $ \ cursor -> Exc.bracket
  (cursor <$ ts_tree_cursor_new_p rootPtr cursor)
  ts_tree_cursor_delete
  action

newtype FieldName = FieldName { getFieldName :: String }
  deriving (Eq, Ord, Show)


class Building a where
  buildNode :: (Alternative m, Carrier sig m, Member (Reader ByteString) sig, Member (Reader (Ptr Cursor)) sig, MonadIO m) => m a
  default buildNode :: (Alternative m, Carrier sig m, GBuilding (Rep a), Generic a, Member (Reader ByteString) sig, Member (Reader (Ptr Cursor)) sig, MonadIO m) => m a
  buildNode = to <$> push (getFields >>= gbuildNode)


instance Building Text.Text where
  buildNode = do
    node <- peekNode
    case node of
      Just node' -> do
        bytestring <- ask
        let start = fromIntegral (nodeStartByte node')
            end = fromIntegral (nodeEndByte node')
        pure (decodeUtf8 (slice start end bytestring))
      _ -> empty

instance Building a => Building (Maybe a) where
  buildNode = Just <$> buildNode

instance (Building a, Building b) => Building (Either a b) where
  buildNode = Left <$> buildNode <|> Right <$> buildNode

instance Building a => Building [a] where
  -- FIXME: this is clearly wrong
  buildNode = pure <$> buildNode


class GBuilding f where
  gbuildNode :: (Alternative m, Carrier sig m, Member (Reader ByteString) sig, Member (Reader (Ptr Cursor)) sig, MonadIO m) => Map.Map FieldName Node -> m (f a)

instance (GBuilding f, GBuilding g) => GBuilding (f :*: g) where
  gbuildNode fields = (:*:) <$> gbuildNode @f fields <*> gbuildNode @g fields

instance (GBuilding f, GBuilding g) => GBuilding (f :+: g) where
  gbuildNode fields = L1 <$> gbuildNode @f fields <|> R1 <$> gbuildNode @g fields

instance GBuilding f => GBuilding (M1 D c f) where
  gbuildNode fields = M1 <$> gbuildNode fields

instance GBuilding f => GBuilding (M1 C c f) where
  gbuildNode fields = M1 <$> gbuildNode fields

instance (GBuilding f, Selector c) => GBuilding (M1 S c f) where
  gbuildNode fields =
    case Map.lookup (FieldName (selName @c undefined)) fields of
      Just node -> do
        goto (nodeTSNode node)
        M1 <$> gbuildNode fields
      Nothing -> empty

instance Building c => GBuilding (K1 i c) where
  gbuildNode _ = K1 <$> buildNode

instance GBuilding U1 where
  gbuildNode _ = pure U1
