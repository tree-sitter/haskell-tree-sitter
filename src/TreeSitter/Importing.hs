{-# LANGUAGE DefaultSignatures, DeriveFunctor, FlexibleContexts, FlexibleInstances, GeneralizedNewtypeDeriving,
             ScopedTypeVariables, TypeApplications, TypeOperators #-}
module TreeSitter.Importing
( parseByteString
, FieldName(..)
, Building(..)
, step
, push
, goto
, peekNode
, peekFieldName
, getFields
) where

import           Control.Applicative
import           Control.Effect hiding ((:+:))
import           Control.Effect.Reader
import           Control.Monad (void)
import           Control.Monad.IO.Class
import           Data.ByteString (ByteString)
import qualified Data.ByteString as B
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
import           TreeSitter.Language as TS
import           TreeSitter.Node as TS
import           TreeSitter.Parser as TS
import           TreeSitter.Tree as TS
import           Data.Proxy

-- Parse source code and produce AST
parseByteString :: Building t => Ptr TS.Language -> ByteString -> IO (Maybe t)
parseByteString language bytestring = withParser language $ \ parser -> withParseTree parser bytestring $ \ treePtr ->
  if treePtr == nullPtr then
    pure Nothing
  else
    withRootNode treePtr $ \ rootPtr ->
      withCursor (castPtr rootPtr) $ \ cursor ->
        runMaybeC (runM (runReader cursor (runReader bytestring buildNode)))

-- | Building is the process of iterating over tree-sitter’s parse trees using its tree cursor API and producing Haskell ASTs for the relevant nodes.
--
--   Datatypes which can be constructed from tree-sitter parse trees may use the default definition of 'buildNode' providing that they have a suitable 'Generic' instance.
class Building a where
  buildNode :: (Alternative m, Carrier sig m, Member (Reader ByteString) sig, Member (Reader (Ptr Cursor)) sig, MonadIO m) => m a
  default buildNode :: (Alternative m, Carrier sig m, GBuilding (Rep a), Generic a, Member (Reader ByteString) sig, Member (Reader (Ptr Cursor)) sig, MonadIO m) => m a
  buildNode = to <$> gbuildNode

  buildEmpty :: Alternative m => m a
  buildEmpty = empty

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
  buildEmpty = pure Nothing

instance (Building a, Building b) => Building (Either a b) where
  buildNode = Left <$> buildNode <|> Right <$> buildNode

instance Building a => Building [a] where
  -- FIXME: this is clearly wrong
  buildNode = pure <$> buildNode
  buildEmpty = pure []


-- | Advance the cursor to the next sibling of the current node.
step :: (Carrier sig m, Member (Reader (Ptr Cursor)) sig, MonadIO m) => m Bool
step = ask >>= liftIO . ts_tree_cursor_goto_next_sibling

-- | Run an action over the children of the current node.
push :: (Carrier sig m, Member (Reader (Ptr Cursor)) sig, MonadIO m) => m a -> m a
push m = do
  void $ ask >>= liftIO . ts_tree_cursor_goto_first_child
  a <- m
  a <$ (ask >>= liftIO . ts_tree_cursor_goto_parent)

-- | Move the cursor to point at the passed 'TSNode'.
goto :: (Carrier sig m, Member (Reader (Ptr Cursor)) sig, MonadIO m) => TSNode -> m ()
goto node = do
  cursor <- ask
  liftIO (with node (ts_tree_cursor_reset_p cursor))

-- | Return the 'Node' that the cursor is pointing at (if any), or 'Nothing' otherwise.
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

-- | Return the field name (if any) for the node that the cursor is pointing at (if any), or 'Nothing' otherwise.
peekFieldName :: (Carrier sig m, Member (Reader (Ptr Cursor)) sig, MonadIO m) => m (Maybe FieldName)
peekFieldName = do
  cursor <- ask
  fieldName <- liftIO $ ts_tree_cursor_current_field_name cursor
  if fieldName == nullPtr then
    pure Nothing
  else
    Just . FieldName <$> liftIO (peekCString fieldName)

-- | Return the fields remaining in the current branch, represented as 'Map.Map' of 'FieldName's to their corresponding 'Node's.
getFields :: (Carrier sig m, Member (Reader (Ptr Cursor)) sig, MonadIO m) => m (Map.Map FieldName Node)
getFields = go Map.empty >>= \fields -> liftIO (print (Map.keys fields)) >> pure fields
  where go fs = do
          node <- peekNode
          case node of
            Just node' -> do
              fieldName <- peekFieldName
              keepGoing <- step
              let fs' = case fieldName of
                    Just fieldName' -> Map.insert fieldName' node' fs
                    _ -> fs
              if keepGoing then go fs'
              else pure fs'
            _ -> pure fs

-- | Return a 'ByteString' that contains a slice of the given 'ByteString'.
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


newtype FieldName = FieldName { getFieldName :: String }
  deriving (Eq, Ord, Show)


-- | Generic construction of ASTs from a 'Map.Map' of named fields.
--
--   Product types (specifically, record types) are constructed by looking up the node for each corresponding field name in the map, moving the cursor to it, and then invoking 'buildNode' to construct the value for that field. Leaf types are constructed as a special case of product types.
--
--   Sum types are constructed by attempting to build each constructor nondeterministically. This should instead use the current node’s symbol to select the corresponding constructor deterministically.
class GBuilding f where
  gbuildNode :: (Alternative m, Carrier sig m, Member (Reader ByteString) sig, Member (Reader (Ptr Cursor)) sig, MonadIO m) => m (f a)
-- we'd only build the map when we know we're looking at a product

instance GBuilding f => GBuilding (M1 D c f) where
  gbuildNode = M1 <$> gbuildNode -- current node, not first child like above in original Building definition

instance GBuilding f => GBuilding (M1 C c f) where
  gbuildNode = M1 <$> gbuildNode

-- Possibly for anonymous leaf nodes:
instance GBuilding U1 where
  gbuildNode = pure U1

-- For regular leaf nodes:
instance (Building k) => GBuilding (M1 S s (K1 c k)) where
  gbuildNode = M1 . K1 <$> buildNode

-- For sum datatypes:
instance (GBuildingSum f, GBuildingSum g) => GBuilding (f :+: g) where
  gbuildNode = push $ gbuildSumNode @(f :+: g) -- FIXME: unclear whether we need to push or not
  --   currentNode <- peekNode
  --   case currentNode of
  --     Just node -> pure $ gbuildSumNode @(f :+: g) node
  --     Nothing -> empty

-- For product datatypes:
instance (GBuildingProduct f, GBuildingProduct g) => GBuilding (f :*: g) where
  gbuildNode = push $ do
    fields <- getFields
    gbuildProductNode @(f :*: g) fields


class GBuildingSum f where
  gbuildSumNode :: (Alternative m
                   , Carrier sig m
                   , Member (Reader ByteString) sig
                   , Member (Reader (Ptr Cursor)) sig
                   , MonadIO m)
                   => m (f a)
-- we'd only build the map when we know we're looking at a product

  gSymbolMatch :: Proxy f -> Node -> Bool

instance Building k => GBuildingSum (M1 C c (M1 S s (K1 i k))) where
  gbuildSumNode = M1 . M1 . K1 <$> buildNode

instance (GBuildingSum f, GBuildingSum g) => GBuildingSum (f :+: g) where
  gbuildSumNode = do
    currentNode <- peekNode
    -- case currentNode of
    --   Just node -> pure $ gSymbolMatch @f undefined node
    --   Nothing -> empty
    let doesSymbolMatchLHS = gSymbolMatch @f undefined currentNode
    if doesSymbolMatchLHS -- FIXME: check both sides and report error
      then L1 <$> gbuildSumNode @f
      else R1 <$> gbuildSumNode @g
  gSymbolMatch _ currentNode = gSymbolMatch @f undefined currentNode || gSymbolMatch @g undefined currentNode

-- | Generically build products
class GBuildingProduct f where
  gbuildProductNode :: (Alternative m, Carrier sig m, Member (Reader ByteString) sig, Member (Reader (Ptr Cursor)) sig, MonadIO m) => Map.Map FieldName Node -> m (f a)

-- Product structure
instance (GBuildingProduct f, GBuildingProduct g) => GBuildingProduct (f :*: g) where
  gbuildProductNode fields = (:*:) <$> gbuildProductNode @f fields <*> gbuildProductNode @g fields

-- Contents of product types (ie., the leaves of the product tree)
instance (Building k, Selector c) => GBuildingProduct (M1 S c (K1 i k)) where
  gbuildProductNode fields =
    case Map.lookup (FieldName (selName @c undefined)) fields of
      Just node -> do
        goto (nodeTSNode node)
        M1 . K1 <$> buildNode
      Nothing -> M1 . K1 <$> buildEmpty
