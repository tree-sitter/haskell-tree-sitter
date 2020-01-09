{-# LANGUAGE DefaultSignatures   #-}
{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE TupleSections       #-}

module TreeSitter.Unmarshal
( parseByteString
, UnmarshalState(..)
, UnmarshalError(..)
, FieldName(..)
, Unmarshal(..)
, UnmarshalAnn(..)
, UnmarshalField(..)
, SymbolMatching(..)
, Match(..)
, hoist
, lookupSymbol
, unmarshalNode
, step
, goto
, peekNode
, peekFieldName
) where

import           Control.Algebra (send)
import           Control.Carrier.Reader hiding (asks)
import           Control.Exception
import           Control.Monad.IO.Class
import           Data.ByteString (ByteString)
import qualified Data.ByteString as B
import           Data.Foldable (toList)
import qualified Data.Map as Map
import qualified Data.IntMap as IntMap
import qualified Data.Text as Text
import           Data.Text.Encoding
import           Data.Text.Encoding.Error (lenientDecode)
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
import           Data.Maybe (fromMaybe)
import           Data.List.NonEmpty (NonEmpty (..))

asks :: Has (Reader r) sig m => (r -> r') -> m r'
asks f = send (Ask (pure . f))
{-# INLINE asks #-}

-- Parse source code and produce AST
parseByteString :: (Unmarshal t, UnmarshalAnn a) => Ptr TS.Language -> ByteString -> IO (Either String (t a))
parseByteString language bytestring = withParser language $ \ parser -> withParseTree parser bytestring $ \ treePtr ->
  if treePtr == nullPtr then
    pure (Left "error: didn't get a root node")
  else
    withRootNode treePtr $ \ rootPtr ->
      withCursor (castPtr rootPtr) $ \ cursor ->
        (Right <$> runReader (UnmarshalState bytestring cursor) (peekNode >>= unmarshalNode))
          `catch` (pure . Left . getUnmarshalError)

newtype UnmarshalError = UnmarshalError { getUnmarshalError :: String }
  deriving (Show)

instance Exception UnmarshalError

data UnmarshalState = UnmarshalState
  { source :: {-# UNPACK #-} !ByteString
  , cursor :: {-# UNPACK #-} !(Ptr Cursor)
  }

type MatchM = ReaderC UnmarshalState IO

newtype Match t = Match
  { runMatch :: forall a . UnmarshalAnn a => Node -> MatchM (t a)
  }

newtype B a = B (forall r . (r -> r -> r) -> (a -> r) -> r -> r)

instance Functor B where
  fmap f (B run) = B (\ fork leaf -> run fork (leaf . f))
  {-# INLINE fmap #-}
  a <$ B run = B (\ fork leaf -> run fork (leaf . const a))
  {-# INLINE (<$) #-}

instance Semigroup (B a) where
  B l <> B r = B (\ fork leaf nil -> fork (l fork leaf nil) (r fork leaf nil))
  {-# INLINE (<>) #-}

instance Monoid (B a) where
  mempty = B (\ _ _ nil -> nil)
  {-# INLINE mempty #-}

instance Foldable B where
  foldMap f (B run) = run (<>) f mempty
  {-# INLINE foldMap #-}

singleton :: a -> B a
singleton a = B (\ _ leaf _ -> leaf a)
{-# INLINE singleton #-}

hoist :: (forall x . t x -> t' x) -> Match t -> Match t'
hoist f (Match run) = Match (fmap f . run)
{-# INLINE hoist #-}

lookupSymbol :: TSSymbol -> IntMap.IntMap a -> Maybe a
lookupSymbol sym map = IntMap.lookup (fromIntegral sym) map
{-# INLINE lookupSymbol #-}

-- | Unmarshal a node
unmarshalNode :: forall t a .
                 ( UnmarshalAnn a
                 , Unmarshal t
                 )
  => Node
  -> MatchM (t a)
unmarshalNode node = case lookupSymbol (nodeSymbol node) matchers' of
  Just t -> runMatch t node
  Nothing -> liftIO . throwIO . UnmarshalError $ showFailure (Proxy @t) node
{-# INLINE unmarshalNode #-}

-- | Unmarshalling is the process of iterating over tree-sitter’s parse trees using its tree cursor API and producing Haskell ASTs for the relevant nodes.
--
--   Datatypes which can be constructed from tree-sitter parse trees may use the default definition of 'matchers' providing that they have a suitable 'Generic1' instance.
class SymbolMatching t => Unmarshal t where
  matchers' :: IntMap.IntMap (Match t)
  matchers' = IntMap.fromList (toList matchers)

  matchers :: B (Int, Match t)
  default matchers :: (Generic1 t, GUnmarshal (Rep1 t)) => B (Int, Match t)
  matchers = foldMap (singleton . (, match)) (matchedSymbols (Proxy @t))
    where match = Match $ \ node -> do
            goto (nodeTSNode node)
            fmap to1 (gunmarshalNode node)

instance (Unmarshal f, Unmarshal g) => Unmarshal (f :+: g) where
  matchers = fmap (fmap (hoist L1)) matchers <> fmap (fmap (hoist R1)) matchers

instance Unmarshal t => Unmarshal (Rec1 t) where
  matchers = fmap (fmap (hoist Rec1)) matchers

instance (KnownNat n, KnownSymbol sym) => Unmarshal (Token sym n) where
  matchers = singleton (fromIntegral (natVal (Proxy @n)), Match (fmap Token . unmarshalAnn))


-- | Unmarshal an annotation field.
--
--   Leaf nodes have 'Text.Text' fields, and leaves, anonymous leaves, and products all have parametric annotation fields. All of these fields are unmarshalled using the metadata of the node, e.g. its start/end bytes, without reference to any child nodes it may contain.
class UnmarshalAnn a where
  unmarshalAnn
    :: Node
    -> MatchM a

instance UnmarshalAnn () where
  unmarshalAnn _ = pure ()

instance UnmarshalAnn Text.Text where
  unmarshalAnn node = do
    range <- unmarshalAnn node
    asks (decodeUtf8With lenientDecode . slice range . source)

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
    :: ( Unmarshal f
       , UnmarshalAnn a
       )
    => [Node]
    -> MatchM (t (f a))

instance UnmarshalField Maybe where
  unmarshalField []  = pure Nothing
  unmarshalField [x] = Just <$> unmarshalNode x
  unmarshalField _   = liftIO . throwIO $ UnmarshalError "expected a node of type (Maybe a) but got multiple"

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
  unmarshalField [] = liftIO . throwIO $ UnmarshalError "expected a node of type (NonEmpty a) but got an empty list"

class SymbolMatching (a :: * -> *) where
  matchedSymbols :: Proxy a -> [Int]

  -- | Provide error message describing the node symbol vs. the symbols this can match
  showFailure :: Proxy a -> Node -> String

instance SymbolMatching f => SymbolMatching (M1 i c f) where
  matchedSymbols _ = matchedSymbols (Proxy @f)
  showFailure _ = showFailure (Proxy @f)

instance SymbolMatching f => SymbolMatching (Rec1 f) where
  matchedSymbols _ = matchedSymbols (Proxy @f)
  showFailure _ = showFailure (Proxy @f)

instance (KnownNat n, KnownSymbol sym) => SymbolMatching (Token sym n) where
  matchedSymbols _ = [fromIntegral (natVal (Proxy @n))]
  showFailure _ _ = "expected " ++ symbolVal (Proxy @sym)

instance (SymbolMatching f, SymbolMatching g) => SymbolMatching (f :+: g) where
  matchedSymbols _ = matchedSymbols (Proxy @f) <> matchedSymbols (Proxy @g)
  showFailure _ = sep <$> showFailure (Proxy @f) <*> showFailure (Proxy @g)

sep :: String -> String -> String
sep a b = a ++ ". " ++ b

-- | Advance the cursor to the next sibling of the current node.
step :: MatchM Bool
step = asks cursor >>= liftIO . ts_tree_cursor_goto_next_sibling
{-# INLINE step #-}

-- | Move the cursor to point at the passed 'TSNode'.
goto :: TSNode -> MatchM ()
goto node = asks cursor >>= liftIO . with node . ts_tree_cursor_reset_p
{-# INLINE goto #-}

-- | Return the 'Node' that the cursor is pointing at.
peekNode :: MatchM Node
peekNode = do
  cursor <- asks cursor
  liftIO $ alloca $ \ tsNodePtr -> do
    _ <- ts_tree_cursor_current_node_p cursor tsNodePtr
    alloca $ \ nodePtr -> do
      ts_node_poke_p tsNodePtr nodePtr
      peek nodePtr

-- | Return the field name (if any) for the node that the cursor is pointing at (if any), or 'Nothing' otherwise.
peekFieldName :: MatchM (Maybe FieldName)
peekFieldName = do
  cursor <- asks cursor
  fieldName <- liftIO $ ts_tree_cursor_current_field_name cursor
  if fieldName == nullPtr then
    pure Nothing
  else
    Just . FieldName . toHaskellCamelCaseIdentifier <$> liftIO (peekCString fieldName)


type Fields = Map.Map FieldName [Node]

-- | Return the fields remaining in the current branch, represented as 'Map.Map' of 'FieldName's to their corresponding 'Node's.
getFields :: MatchM Fields
getFields = do
  hasChildren <- asks cursor >>= liftIO . ts_tree_cursor_goto_first_child
  if hasChildren then
    go Map.empty <* (asks cursor >>= liftIO . ts_tree_cursor_goto_parent)
  else
    pure Map.empty
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
--   Sum types are constructed by using the current node’s symbol to select the corresponding constructor deterministically.
class GUnmarshal f where
  gunmarshalNode
    :: UnmarshalAnn a
    => Node
    -> MatchM (f a)

instance GUnmarshal f => GUnmarshal (M1 D c f) where
  gunmarshalNode node = M1 <$> gunmarshalNode node

instance GUnmarshal f => GUnmarshal (M1 C c f) where
  gunmarshalNode node = M1 <$> gunmarshalNode node

instance GUnmarshal f => GUnmarshal (M1 S c f) where
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

instance Unmarshal t => GUnmarshal (Rec1 t) where
  gunmarshalNode node = Rec1 <$> unmarshalNode node

-- For product datatypes:
instance (GUnmarshalProduct f, GUnmarshalProduct g) => GUnmarshal (f :*: g) where
  gunmarshalNode node = getFields >>= gunmarshalProductNode @(f :*: g) node


-- | Generically unmarshal products
class GUnmarshalProduct f where
  gunmarshalProductNode
    :: UnmarshalAnn a
    => Node
    -> Fields
    -> MatchM (f a)

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
      []  -> liftIO . throwIO . UnmarshalError $ "expected a node '" <> selName @c undefined <> "' but didn't get one"
      [x] -> M1 . Rec1 <$> unmarshalNode x
      _   -> liftIO . throwIO . UnmarshalError $ "expected a node but got multiple"
