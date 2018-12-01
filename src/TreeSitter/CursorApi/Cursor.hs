{-# LANGUAGE DeriveGeneric, InterruptibleFFI, RankNTypes, RecordWildCards #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}

module TreeSitter.CursorApi.Cursor (
  Cursor(..),
  SpanInfo(..)
  , tsTransformSpanInfos
  , tsTransformZipper
  , tsTransformIdentityZipper
  , hts_parse_with_language
  , ts_cursor_init
  , ts_cursor_reset_root
  , ts_cursor_goto_first_child
  , ts_cursor_goto_next_sibling
  , ts_cursor_goto_parent
  , funptr_ts_cursor_free
) where

import           Foreign
import           Foreign.Ptr
import           Foreign.C
import           Foreign.C.Types
import           Foreign.Marshal.Utils
import           GHC.Generics

import           TreeSitter.Language
import           TreeSitter.Tree
import           TreeSitter.Struct
import           TreeSitter.TsPoint
import           TreeSitter.CursorApi.Types

import qualified Data.Tree                     as T
import qualified Data.Tree.Zipper              as Z
import           Data.Maybe
import           Control.Monad.Identity

-- TODO remove all TSPoint (also in C counterpart!)
data Cursor = Cursor
  { nodeType :: !CString
  , nodeSymbol :: !Word16
  , nodeStartPoint :: !TSPoint
  , nodeEndPoint :: !TSPoint
  , nodeStartByte :: !Word32
  , nodeEndByte :: !Word32
  }
  deriving (Show, Eq, Generic)

instance Storable Cursor where
  alignment _ = alignment (nullPtr :: Ptr ())
  sizeOf _ = 36
  peek = evalStruct $ Cursor <$> peekStruct
                             <*> peekStruct
                             <*> peekStruct
                             <*> peekStruct
                             <*> peekStruct
                             <*> peekStruct
  poke _ _ = error "Cant poke"

type PtrCursor = Ptr Cursor

data Navigation = Down | Next | Up

data CursorOperations a m c = CursorOperations 
                      { initResult     :: Monad m => a -> m c
                      , packNode       :: Monad m => a -> Navigation -> c -> m c
                      , nodeFirstChild :: Monad m => (a -> m (Maybe a))
                      , nodeNext       :: Monad m => (a -> m (Maybe a))
                      , nodeParent     :: Monad m => (a -> m (Maybe a))
                      }


spanInfoFromCursor :: PtrCursor -> IO SpanInfo
spanInfoFromCursor cur = do
  isParent <- hasChildren
  Cursor{..} <- peek cur
  tokentype <- peekCString nodeType
  let startByte = fromIntegral nodeStartByte
      endByte   = fromIntegral nodeEndByte
    in return (if isParent then Parent startByte endByte tokentype else Token startByte endByte tokentype)

-- transformations

tsTransformSpanInfos :: PtrCursor -> IO [SpanInfo]
tsTransformSpanInfos = tsTransform curopsList

packNodeList :: PtrCursor -> Navigation -> [SpanInfo] -> IO [SpanInfo]
packNodeList ptrCur nav spanInfos = 
  case nav of
      Down -> do
        spanInfo <- spanInfoFromCursor ptrCur
        return $ spanInfo : spanInfos

      Next -> do
        spanInfo <- spanInfoFromCursor ptrCur
        return $ spanInfo : spanInfos

      Up -> return spanInfos


initList :: PtrCursor -> IO [SpanInfo]
initList ptrCur = do
  spanInfo <- spanInfoFromCursor ptrCur
  return [spanInfo]

curopsList :: CursorOperations PtrCursor IO [SpanInfo]
curopsList = CursorOperations
            { initResult     = initList
            , packNode       = packNodeList
            , nodeFirstChild = firstChild
            , nodeNext       = next
            , nodeParent     = parent
            }


tsTransformZipper :: PtrCursor -> IO (Z.TreePos Z.Full String)
tsTransformZipper = tsTransform curopsZipper

packNodeZipper :: PtrCursor -> Navigation -> Z.TreePos Z.Full String -> IO (Z.TreePos Z.Full String)
packNodeZipper ptrCur nav resultZipper =
  case nav of
      Down -> do
        l <- label ptrCur
        return $ insertNode l (Z.children resultZipper)

      Next -> do
        l <- label ptrCur
        return $ insertNode l (Z.nextSpace resultZipper)

      Up -> return (fromJust $ Z.parent resultZipper)

  where
    insertNode lbl = Z.insert (T.Node lbl [])

initZipper :: PtrCursor -> IO (Z.TreePos Z.Full String)
initZipper ptrCur = do
  rootLabel <- label ptrCur
  return $ Z.fromTree (T.Node rootLabel [])

curopsZipper :: CursorOperations PtrCursor IO (Z.TreePos Z.Full String)
curopsZipper = CursorOperations
            { initResult     = initZipper
            , packNode       = packNodeZipper
            , nodeFirstChild = firstChild
            , nodeNext       = next
            , nodeParent     = parent
            }


tsTransformIdentityZipper :: Z.TreePos Z.Full String -> Identity (Z.TreePos Z.Full String)
tsTransformIdentityZipper = tsTransform curopsIdentityZipper

packNodeIdentityZipper :: Z.TreePos Z.Full String -> Navigation -> Z.TreePos Z.Full String -> Identity (Z.TreePos Z.Full String)
packNodeIdentityZipper ptrCur nav resultZipper =
  case nav of
    Down -> return $ insertNode (Z.label ptrCur) (Z.children resultZipper)
    Next -> return $ insertNode (Z.label ptrCur) (Z.nextSpace resultZipper)
    Up -> return (fromJust $ Z.parent resultZipper)
  where
    insertNode lbl = Z.insert (T.Node lbl [])

initIdentityZipper :: Z.TreePos Z.Full String -> Identity (Z.TreePos Z.Full String)
initIdentityZipper ptrCur = return $ Z.fromTree (T.Node (Z.label ptrCur) [])

curopsIdentityZipper :: CursorOperations (Z.TreePos Z.Full String) Identity (Z.TreePos Z.Full String)
curopsIdentityZipper = CursorOperations
            { initResult     = initIdentityZipper
            , packNode       = packNodeIdentityZipper
            , nodeFirstChild = return . Z.firstChild
            , nodeNext       = return . Z.next
            , nodeParent     = return . Z.parent
            }


tsTransform :: Monad m => CursorOperations a m c -> a -> m c
tsTransform curops@CursorOperations{..} ptrCur = do
  res <- initResult ptrCur
  go curops res Down ptrCur
  where
    go :: Monad m => CursorOperations a m c -> c -> Navigation -> a -> m c
    go curops@CursorOperations{..} res nav ptrCur = case nav of
      Down -> do
        fc <- nodeFirstChild ptrCur
        case fc of
          Nothing      -> go curops res Next ptrCur
          Just ptrCur' -> do
            res' <- packNode ptrCur' Down res
            go curops res' Down ptrCur'
      
      Next -> do
        n <- nodeNext ptrCur
        case n of
          Nothing      -> go curops res Up ptrCur
          Just ptrCur' -> do
            res' <- packNode ptrCur' Next res
            go curops res' Down ptrCur'

      Up -> do
        p <- nodeParent ptrCur
        case p of
          Nothing      -> return res
          Just ptrCur' -> do
            r <- packNode ptrCur' Up res
            go curops r Next ptrCur'



label :: PtrCursor -> IO String
label cur = do
  node <- peek cur
  nodeType <- peekCString (nodeType node)
  let nodeStart = nodeStartPoint node
      nodeEnd   = nodeEndPoint node
      startPoint = show (pointRow nodeStart, pointColumn nodeStart)
      endPoint = show (pointRow nodeEnd, pointColumn nodeEnd)
    in      
    return $ nodeType ++ " " ++ startPoint ++ "-" ++ endPoint

firstChild :: PtrCursor -> IO (Maybe PtrCursor)
firstChild cur = do
  exists <- ts_cursor_goto_first_child cur
  return $ boolToMaybe cur exists

next :: PtrCursor -> IO (Maybe PtrCursor)
next cur = do
  exists <- ts_cursor_goto_next_sibling cur
  return $ boolToMaybe cur exists

parent :: PtrCursor -> IO (Maybe PtrCursor)
parent cur = do
  exists <- ts_cursor_goto_parent cur
  return $ boolToMaybe cur exists

hasChildren :: IO Bool
hasChildren = toBool <$> ts_cursor_has_children

boolToMaybe :: PtrCursor -> CBool  -> Maybe PtrCursor
boolToMaybe cur exists =
  if exists == 1
    then Just cur
    else Nothing



foreign import ccall hts_parse_with_language :: Ptr Language -> CString -> Word32 -> IO (Ptr Tree)
foreign import ccall ts_cursor_init :: Ptr Tree -> PtrCursor -> IO ()
foreign import ccall ts_cursor_reset_root :: Ptr Tree -> PtrCursor -> IO ()
foreign import ccall ts_cursor_goto_first_child :: PtrCursor -> IO CBool
foreign import ccall ts_cursor_goto_next_sibling :: PtrCursor -> IO CBool
foreign import ccall ts_cursor_goto_parent :: PtrCursor -> IO CBool
foreign import ccall ts_cursor_has_children :: IO CBool
foreign import ccall "&ts_cursor_free" funptr_ts_cursor_free :: FunPtr (PtrCursor -> IO ())
