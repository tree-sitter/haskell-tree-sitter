{-# LANGUAGE DefaultSignatures, FlexibleContexts, FlexibleInstances,
             PolyKinds, ScopedTypeVariables, TypeApplications, TypeOperators #-}
module TreeSitter.Unmarshal
( parseByteString
, FieldName(..)
, Unmarshal(..)
, SymbolMatching(..)
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
import           Control.Effect.Fail
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
import           TreeSitter.Span as TS
import           TreeSitter.Range as TS
import           Data.Proxy
import           Prelude hiding (fail)
import           Data.Maybe (fromMaybe, maybeToList)
import           Data.List.NonEmpty (NonEmpty (..))

-- Parse source code and produce AST
parseByteString :: Unmarshal t => Ptr TS.Language -> ByteString -> IO (Either String t)
parseByteString language bytestring = withParser language $ \ parser -> withParseTree parser bytestring $ \ treePtr ->
  if treePtr == nullPtr then
    pure (Left "error: didn't get a root node")
  else
    withRootNode treePtr $ \ rootPtr ->
      withCursor (castPtr rootPtr) $ \ cursor ->
        runM (runFail (runReader cursor (runReader bytestring (peekNode >>= unmarshalNodes . maybeToList))))

-- | Unmarshal is the process of iterating over tree-sitter’s parse trees using its tree cursor API and producing Haskell ASTs for the relevant nodes.
--
--   Datatypes which can be constructed from tree-sitter parse trees may use the default definition of 'unmarshalNode' providing that they have a suitable 'Generic' instance.
class Unmarshal a where

  unmarshalNodes :: (MonadFail m, Carrier sig m, Member (Reader ByteString) sig, Member (Reader (Ptr Cursor)) sig, MonadIO m) => [Node] -> m a
  default unmarshalNodes :: (MonadFail m, Carrier sig m, GUnmarshal (Rep a), Generic a, Member (Reader ByteString) sig, Member (Reader (Ptr Cursor)) sig, MonadIO m) => [Node] -> m a
  unmarshalNodes [x] = do
    goto (nodeTSNode x)
    to <$> gunmarshalNode x
  unmarshalNodes [] = fail "expected a node but didn't get one"
  unmarshalNodes _ = fail "expected a node but got multiple"

instance Unmarshal () where
  unmarshalNodes _ = pure ()

instance Unmarshal Text.Text where
  unmarshalNodes _ = do
    node <- peekNode
    bytestring <- ask
    case node of
      Just node -> do
        let start = fromIntegral (nodeStartByte node)
            end = fromIntegral (nodeEndByte node)
        pure (decodeUtf8 (slice start end bytestring))
      Nothing -> fail "expected a node but didn't get one"

-- | Instance for pairs of annotations
instance (Unmarshal a, Unmarshal b) => Unmarshal (a,b) where
  unmarshalNodes listofNodes = (,) <$> unmarshalNodes @a listofNodes <*> unmarshalNodes @b listofNodes

instance Unmarshal Range where
  unmarshalNodes _ = do
    node <- peekNode
    case node of
      Just node -> do
        let start = fromIntegral (nodeStartByte node)
            end = fromIntegral (nodeEndByte node)
        pure (Range start end)
      Nothing -> fail "expected a node but didn't get one"

instance Unmarshal Span where
  unmarshalNodes _ = do
    node <- peekNode
    case node of
      Just node -> do
        let spanStart = pointToPos (nodeStartPoint node)
            spanEnd = pointToPos (nodeEndPoint node)
        pure (Span spanStart spanEnd)
      Nothing -> fail "expected a node but didn't get one"

pointToPos :: TSPoint -> Pos
pointToPos (TSPoint line column) = Pos (fromIntegral line) (fromIntegral column)

instance Unmarshal a => Unmarshal (Maybe a) where
  unmarshalNodes [] = pure Nothing
  unmarshalNodes listOfNodes = Just <$> unmarshalNodes listOfNodes

instance (Unmarshal a, Unmarshal b, SymbolMatching a, SymbolMatching b) => Unmarshal (Either a b) where
  unmarshalNodes [node] = do
    let lhsSymbolMatch = symbolMatch (Proxy @a) node
        rhsSymbolMatch = symbolMatch (Proxy @b) node
    if lhsSymbolMatch
      then Left <$> unmarshalNodes @a [node]
      else if rhsSymbolMatch
        then Right <$> unmarshalNodes @b [node]
        else fail $ showFailure (Proxy @(Either a b)) node
  unmarshalNodes [] = fail "expected a node of type (Either a b) but didn't get one"
  unmarshalNodes _ = fail "expected a node of type (Either a b) but got multiple"


instance Unmarshal a => Unmarshal [a] where
  unmarshalNodes (x:xs) = do
    head' <- unmarshalNodes [x]
    tail' <- unmarshalNodes xs
    pure $ head' : tail'
  unmarshalNodes [] = pure []


instance Unmarshal a => Unmarshal (NonEmpty a) where
  unmarshalNodes (x:xs) = do
    head' <- unmarshalNodes [x]
    tail' <- unmarshalNodes xs
    pure $ head' :| tail'
  unmarshalNodes [] = fail "expected a node but didn't get one"

class SymbolMatching a where
  symbolMatch :: Proxy a -> Node -> Bool

  -- | Provide error message describing the node symbol vs. the symbols this can match
  showFailure :: Proxy a -> Node -> String

instance SymbolMatching a => SymbolMatching (Maybe a) where
  symbolMatch _ = symbolMatch (Proxy @a)
  showFailure _ = showFailure (Proxy @a)

instance (SymbolMatching a, SymbolMatching b) => SymbolMatching (Either a b) where
  symbolMatch _ = (||) <$> symbolMatch (Proxy @a) <*> symbolMatch (Proxy @b)
  showFailure _ = sep <$> showFailure (Proxy @a) <*> showFailure (Proxy @b)

instance SymbolMatching a => SymbolMatching [a] where
  symbolMatch _ = symbolMatch (Proxy @a)
  showFailure _ = showFailure (Proxy @a)

instance SymbolMatching k => SymbolMatching (M1 C c (M1 S s (K1 i k))) where
  symbolMatch _ = symbolMatch (Proxy @k)
  showFailure _ = showFailure (Proxy @k)

instance (SymbolMatching f, SymbolMatching g) => SymbolMatching (f :+: g) where
  symbolMatch _ = (||) <$> symbolMatch (Proxy @f) <*> symbolMatch (Proxy @g)
  showFailure _ = sep <$> showFailure (Proxy @f) <*> showFailure (Proxy @g)

sep :: String -> String -> String
sep a b = a ++ ". " ++ b

-- | Advance the cursor to the next sibling of the current node.
step :: (Carrier sig m, Member (Reader (Ptr Cursor)) sig, MonadIO m) => m Bool
step = ask >>= liftIO . ts_tree_cursor_goto_next_sibling

-- | Run an action over the children of the current node.
push :: (Carrier sig m, Member (Reader (Ptr Cursor)) sig, MonadIO m) => m a -> m (Maybe a)
push m = do
  hasChildren <- ask >>= liftIO . ts_tree_cursor_goto_first_child
  if hasChildren then do
    a <- m
    Just a <$ (ask >>= liftIO . ts_tree_cursor_goto_parent)
  else
    pure Nothing

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
getFields :: (Carrier sig m, Member (Reader (Ptr Cursor)) sig, MonadIO m) => m (Map.Map FieldName [Node])
getFields = go Map.empty -- >>= \fields -> liftIO (print (Map.keys fields)) >> pure fields
  where go fs = do
          node <- peekNode
          case node of
            Just node' -> do
              fieldName <- peekFieldName
              keepGoing <- step
              let fs' = case fieldName of
                    Just fieldName' -> Map.insertWith (flip (++)) fieldName' [node'] fs
                    _ -> if nodeIsNamed node' /= 0
                      then Map.insertWith (flip (++)) (FieldName "extraChildren") [node'] fs
                      else fs
              if keepGoing then go fs'
              else pure fs'
            _ -> pure fs

-- | Return a 'ByteString' that contains a slice of the given 'ByteString'.
slice :: Int -> Int -> ByteString -> ByteString
slice start end = take . drop
  where drop = B.drop start
        take = B.take (end - start)


newtype FieldName = FieldName { getFieldName :: String }
  deriving (Eq, Ord, Show)


-- | Generic construction of ASTs from a 'Map.Map' of named fields.
--
--   Product types (specifically, record types) are constructed by looking up the node for each corresponding field name in the map, moving the cursor to it, and then invoking 'unmarshalNode' to construct the value for that field. Leaf types are constructed as a special case of product types.
--
--   Sum types are constructed by attempting to unmarshal each constructor nondeterministically. This should instead use the current node’s symbol to select the corresponding constructor deterministically.
class GUnmarshal f where
  gunmarshalNode :: (MonadFail m, Carrier sig m, Member (Reader ByteString) sig, Member (Reader (Ptr Cursor)) sig, MonadIO m) => Node -> m (f a)

instance GUnmarshal f => GUnmarshal (M1 D c f) where
  gunmarshalNode node = M1 <$> gunmarshalNode node

instance GUnmarshal f => GUnmarshal (M1 C c f) where
  gunmarshalNode node = M1 <$> gunmarshalNode node

-- For anonymous leaf nodes:
instance GUnmarshal U1 where
  gunmarshalNode _ = pure U1


-- For unary products:
instance (Selector s, Unmarshal k) => GUnmarshal (M1 S s (K1 c k)) where
  gunmarshalNode _ = push getFields >>= gunmarshalProductNode . fromMaybe Map.empty

-- For sum datatypes:
instance (GUnmarshalSum f, GUnmarshalSum g, SymbolMatching f, SymbolMatching g) => GUnmarshal (f :+: g) where
  gunmarshalNode = gunmarshalSumNode @(f :+: g)

-- For product datatypes:
instance (GUnmarshalProduct f, GUnmarshalProduct g) => GUnmarshal (f :*: g) where
  gunmarshalNode _ = push getFields >>= gunmarshalProductNode @(f :*: g) . fromMaybe Map.empty

class GUnmarshalSum f where
  gunmarshalSumNode :: (MonadFail m
                   , Carrier sig m
                   , Member (Reader ByteString) sig
                   , Member (Reader (Ptr Cursor)) sig
                   , MonadIO m)
                   => Node -> m (f a)

instance (Unmarshal k, SymbolMatching k) => GUnmarshalSum (M1 C c (M1 S s (K1 i k))) where
  gunmarshalSumNode node = M1 . M1 . K1 <$> unmarshalNodes [node]

instance (GUnmarshalSum f, GUnmarshalSum g, SymbolMatching f, SymbolMatching g) => GUnmarshalSum (f :+: g) where
  gunmarshalSumNode node = do
    let lhsSymbolMatch = symbolMatch (Proxy @f) node
        rhsSymbolMatch = symbolMatch (Proxy @g) node
    if lhsSymbolMatch
      then L1 <$> gunmarshalSumNode @f node
      else if rhsSymbolMatch
        then R1 <$> gunmarshalSumNode @g node
        else fail $ showFailure (Proxy @f) node `sep` showFailure (Proxy @g) node


-- | Generically unmarshal products
class GUnmarshalProduct f where
  gunmarshalProductNode :: (MonadFail m, Carrier sig m, Member (Reader ByteString) sig, Member (Reader (Ptr Cursor)) sig, MonadIO m) => Map.Map FieldName [Node] -> m (f a)

-- Product structure
instance (GUnmarshalProduct f, GUnmarshalProduct g) => GUnmarshalProduct (f :*: g) where
  gunmarshalProductNode fields = (:*:) <$> gunmarshalProductNode @f fields <*> gunmarshalProductNode @g fields

-- Contents of product types (ie., the leaves of the product tree)
instance (Unmarshal k, Selector c) => GUnmarshalProduct (M1 S c (K1 i k)) where
  gunmarshalProductNode fields =
   M1 . K1 <$> unmarshalNodes (fromMaybe [] (Map.lookup (FieldName (selName @c undefined)) fields))
