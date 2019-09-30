{-# LANGUAGE DefaultSignatures, FlexibleContexts, FlexibleInstances, PolyKinds,
             ScopedTypeVariables, TypeApplications, TypeOperators #-}
module TreeSitter.Unmarshal
( parseByteString
, FieldName(..)
, Unmarshal(..)
, UnmarshalAnn(..)
, UnmarshalField(..)
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
import           GHC.TypeLits
import           TreeSitter.Cursor as TS
import           TreeSitter.Language as TS
import           TreeSitter.Node as TS
import           TreeSitter.Parser as TS
import           TreeSitter.Tree as TS
import           TreeSitter.Token as TS
import           Source.Loc
import           Source.Span
import           Data.Proxy
import           Prelude hiding (fail)
import           Data.Maybe (fromMaybe)
import           Data.List.NonEmpty (NonEmpty (..))

-- Parse source code and produce AST
parseByteString :: (Unmarshal t, UnmarshalAnn a) => Ptr TS.Language -> ByteString -> IO (Either String (t a))
parseByteString language bytestring = withParser language $ \ parser -> withParseTree parser bytestring $ \ treePtr ->
  if treePtr == nullPtr then
    pure (Left "error: didn't get a root node")
  else
    withRootNode treePtr $ \ rootPtr ->
      withCursor (castPtr rootPtr) $ \ cursor ->
        runM (runFail (runReader cursor (runReader bytestring (peekNode >>= unmarshalNode))))

-- | Unmarshalling is the process of iterating over tree-sitter’s parse trees using its tree cursor API and producing Haskell ASTs for the relevant nodes.
--
--   Datatypes which can be constructed from tree-sitter parse trees may use the default definition of 'unmarshalNode' providing that they have a suitable 'Generic1' instance.
class Unmarshal t where
  unmarshalNode
    :: ( Carrier sig m
       , Member (Reader ByteString) sig
       , Member (Reader (Ptr Cursor)) sig
       , MonadFail m
       , MonadIO m
       , UnmarshalAnn a
       )
    => Node
    -> m (t a)
  default unmarshalNode
    :: ( Carrier sig m
       , Generic1 t
       , GUnmarshal (Rep1 t)
       , Member (Reader ByteString) sig
       , Member (Reader (Ptr Cursor)) sig
       , MonadFail m
       , MonadIO m
       , UnmarshalAnn a
       )
    => Node
    -> m (t a)
  unmarshalNode x = do
    goto (nodeTSNode x)
    to1 <$> gunmarshalNode x

instance (Unmarshal f, Unmarshal g, SymbolMatching f, SymbolMatching g) => Unmarshal (f :+: g) where
  unmarshalNode node = do
    let lhsSymbolMatch = symbolMatch (Proxy @f) node
        rhsSymbolMatch = symbolMatch (Proxy @g) node
    if lhsSymbolMatch then
      L1 <$> unmarshalNode @f node
    else if rhsSymbolMatch then
      R1 <$> unmarshalNode @g node
    else
      fail $ showFailure (Proxy @(f :+: g)) node

instance Unmarshal t => Unmarshal (Rec1 t) where
  unmarshalNode = fmap Rec1 . unmarshalNode

instance Unmarshal (Token sym n) where
  unmarshalNode = fmap Token . unmarshalAnn


-- | Unmarshal an annotation field.
--
--   Leaf nodes have 'Text.Text' fields, and leaves, anonymous leaves, and products all have parametric annotation fields. All of these fields are unmarshalled using the metadata of the node, e.g. its start/end bytes, without reference to any child nodes it may contain.
class UnmarshalAnn a where
  unmarshalAnn
    :: ( Carrier sig m
       , Member (Reader ByteString) sig
       , Member (Reader (Ptr Cursor)) sig
       , MonadFail m
       , MonadIO m
       )
    => Node
    -> m a

instance UnmarshalAnn () where
  unmarshalAnn _ = pure ()

instance UnmarshalAnn Text.Text where
  unmarshalAnn node = do
    range <- unmarshalAnn node
    bytestring <- ask
    pure (decodeUtf8 (slice range bytestring))

-- | Instance for pairs of annotations
instance (UnmarshalAnn a, UnmarshalAnn b) => UnmarshalAnn (a,b) where
  unmarshalAnn node = (,)
    <$> unmarshalAnn @a node
    <*> unmarshalAnn @b node

instance UnmarshalAnn Loc where
  unmarshalAnn node = Loc
    <$> unmarshalAnn @Range node
    <*> unmarshalAnn @Span  node

instance UnmarshalAnn Range where
  unmarshalAnn node = do
    let start = fromIntegral (nodeStartByte node)
        end   = fromIntegral (nodeEndByte node)
    pure (Range start end)

instance UnmarshalAnn Span where
  unmarshalAnn node = do
    let spanStart = pointToPos (nodeStartPoint node)
        spanEnd   = pointToPos (nodeEndPoint node)
    pure (Span spanStart spanEnd)

pointToPos :: TSPoint -> Pos
pointToPos (TSPoint line column) = Pos (fromIntegral line) (fromIntegral column)


-- | Optional/repeated fields occurring in product datatypes are wrapped in type constructors, e.g. 'Maybe', '[]', or 'NonEmpty', and thus can unmarshal zero or more nodes for the same field name.
class UnmarshalField t where
  unmarshalField
    :: ( Carrier sig m
       , Member (Reader ByteString) sig
       , Member (Reader (Ptr Cursor)) sig
       , MonadFail m
       , MonadIO m
       , Unmarshal f
       , UnmarshalAnn a
       )
    => [Node]
    -> m (t (f a))

instance UnmarshalField Maybe where
  unmarshalField []  = pure Nothing
  unmarshalField [x] = Just <$> unmarshalNode x
  unmarshalField _   = fail "expected a node of type ((f :+: g) a) but got multiple"

instance UnmarshalField [] where
  unmarshalField (x:xs) = do
    head' <- unmarshalNode x
    tail' <- unmarshalField xs
    pure $ head' : tail'
  unmarshalField [] = pure []

instance UnmarshalField NonEmpty where
  unmarshalField (x:xs) = do
    head' <- unmarshalNode x
    tail' <- unmarshalField xs
    pure $ head' :| tail'
  unmarshalField [] = fail "expected a node but didn't get one"


class SymbolMatching (a :: * -> *) where
  symbolMatch :: Proxy a -> Node -> Bool

  -- | Provide error message describing the node symbol vs. the symbols this can match
  showFailure :: Proxy a -> Node -> String

instance SymbolMatching f => SymbolMatching (M1 i c f) where
  symbolMatch _ = symbolMatch (Proxy @f)
  showFailure _ = showFailure (Proxy @f)

instance SymbolMatching f => SymbolMatching (Rec1 f) where
  symbolMatch _ = symbolMatch (Proxy @f)
  showFailure _ = showFailure (Proxy @f)

instance KnownSymbol sym => SymbolMatching (Token sym n) where
  -- FIXME: this should compare the node’s symbol against @sym@
  symbolMatch _ _ = False
  showFailure _ _ = "expected " ++ symbolVal (Proxy @sym)

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

-- | Return the 'Node' that the cursor is pointing at.
peekNode :: (Carrier sig m, Member (Reader (Ptr Cursor)) sig, MonadIO m) => m Node
peekNode = do
  cursor <- ask
  liftIO $ alloca $ \ tsNodePtr -> do
    _ <- ts_tree_cursor_current_node_p cursor tsNodePtr
    alloca $ \ nodePtr -> do
      ts_node_poke_p tsNodePtr nodePtr
      peek nodePtr

-- | Return the field name (if any) for the node that the cursor is pointing at (if any), or 'Nothing' otherwise.
peekFieldName :: (Carrier sig m, Member (Reader (Ptr Cursor)) sig, MonadIO m) => m (Maybe FieldName)
peekFieldName = do
  cursor <- ask
  fieldName <- liftIO $ ts_tree_cursor_current_field_name cursor
  if fieldName == nullPtr then
    pure Nothing
  else
    Just . FieldName <$> liftIO (peekCString fieldName)


type Fields = Map.Map FieldName [Node]

-- | Return the fields remaining in the current branch, represented as 'Map.Map' of 'FieldName's to their corresponding 'Node's.
getFields :: (Carrier sig m, Member (Reader (Ptr Cursor)) sig, MonadIO m) => m Fields
getFields = go Map.empty
  where go fs = do
          node <- peekNode
          fieldName <- peekFieldName
          keepGoing <- step
          let fs' = case fieldName of
                Just fieldName' -> Map.insertWith (flip (++)) fieldName' [node] fs
                -- NB: We currently skip “extra” nodes (i.e. ones occurring in the @extras@ rule), pending a fix to https://github.com/tree-sitter/haskell-tree-sitter/issues/99
                _ -> if nodeIsNamed node /= 0 && nodeIsExtra node == 0
                  then Map.insertWith (flip (++)) (FieldName "extraChildren") [node] fs
                  else fs
          if keepGoing then go fs'
          else pure fs'

lookupField :: FieldName -> Fields -> [Node]
lookupField k = fromMaybe [] . Map.lookup k


-- | Return a 'ByteString' that contains a slice of the given 'ByteString'.
slice :: Range -> ByteString -> ByteString
slice (Range start end) = take . drop
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
  gunmarshalNode
    :: ( Carrier sig m
       , Member (Reader ByteString) sig
       , Member (Reader (Ptr Cursor)) sig
       , MonadFail m
       , MonadIO m
       , UnmarshalAnn a
       )
    => Node
    -> m (f a)

instance GUnmarshal f => GUnmarshal (M1 i c f) where
  gunmarshalNode node = M1 <$> gunmarshalNode node

-- For anonymous leaf nodes:
instance GUnmarshal U1 where
  gunmarshalNode _ = pure U1


-- For unary products:
instance UnmarshalAnn k => GUnmarshal (K1 c k) where
  gunmarshalNode node = K1 <$> unmarshalAnn node

-- For anonymous leaf nodes
instance GUnmarshal Par1 where
  gunmarshalNode node = Par1 <$> unmarshalAnn node

instance (Unmarshal t, SymbolMatching t) => GUnmarshal (Rec1 t) where
  gunmarshalNode node = Rec1 <$> unmarshalNode node

-- For product datatypes:
instance (GUnmarshalProduct f, GUnmarshalProduct g) => GUnmarshal (f :*: g) where
  gunmarshalNode node = push getFields >>= gunmarshalProductNode @(f :*: g) node . fromMaybe Map.empty

-- For sum datatypes:
instance (GUnmarshal f, GUnmarshal g, SymbolMatching f, SymbolMatching g) => GUnmarshal (f :+: g) where
  gunmarshalNode node = do
    let lhsSymbolMatch = symbolMatch (Proxy @f) node
        rhsSymbolMatch = symbolMatch (Proxy @g) node
    if lhsSymbolMatch then
      L1 <$> gunmarshalNode @f node
    else if rhsSymbolMatch then
      R1 <$> gunmarshalNode @g node
    else
      fail $ showFailure (Proxy @f) node `sep` showFailure (Proxy @g) node


-- | Generically unmarshal products
class GUnmarshalProduct f where
  gunmarshalProductNode
    :: ( Carrier sig m
       , Member (Reader ByteString) sig
       , Member (Reader (Ptr Cursor)) sig
       , MonadFail m
       , MonadIO m
       , UnmarshalAnn a
       )
    => Node
    -> Fields
    -> m (f a)

-- Product structure
instance (GUnmarshalProduct f, GUnmarshalProduct g) => GUnmarshalProduct (f :*: g) where
  gunmarshalProductNode node fields = (:*:)
    <$> gunmarshalProductNode @f node fields
    <*> gunmarshalProductNode @g node fields

-- Contents of product types (ie., the leaves of the product tree)
instance UnmarshalAnn k => GUnmarshalProduct (M1 S c (K1 i k)) where
  gunmarshalProductNode node _ = M1 . K1 <$> unmarshalAnn node

instance GUnmarshalProduct (M1 S c Par1) where
  gunmarshalProductNode node _ = M1 . Par1 <$> unmarshalAnn node

instance (UnmarshalField f, Unmarshal g, Selector c) => GUnmarshalProduct (M1 S c (f :.: g)) where
  gunmarshalProductNode _ fields =
    M1 . Comp1 <$> unmarshalField (lookupField (FieldName (selName @c undefined)) fields)

instance (Unmarshal t, Selector c) => GUnmarshalProduct (M1 S c (Rec1 t)) where
  gunmarshalProductNode _ fields =
    case lookupField (FieldName (selName @c undefined)) fields of
      []  -> fail "expected a node but didn't get one"
      [x] -> M1 . Rec1 <$> unmarshalNode x
      _   -> fail "expected a node but got multiple"
