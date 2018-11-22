-- {-# LANGUAGE DeriveGeneric, DeriveAnyClass, InterruptibleFFI, RankNTypes, ScopedTypeVariables, FlexibleInstances, MultiParamTypeClasses #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass, InterruptibleFFI, RankNTypes, GADTs, ScopedTypeVariables #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}
module TreeSitter.Cursor (
  Cursor(..),
  SpanInfo(..)
  , tsTransformTree
  , helpersIO
  , helpersID
  , tsTransformList
  , ts_cursor_init  
  , ts_cursor_goto_first_child
  , ts_cursor_goto_next_sibling
  , ts_cursor_goto_parent
  , funptr_ts_cursor_free
) where

import Foreign
import Foreign.Ptr
import Foreign.C
import Foreign.C.Types
import Foreign.Marshal.Utils
import GHC.Generics

import TreeSitter.Tree
import TreeSitter.Struct

import qualified Data.Tree as T
import qualified Data.Tree.Zipper as Z
import Data.Maybe
import Control.Monad.Identity

import Data.Loc.Span hiding ((+))
import Data.Loc.Loc
import Data.Loc.Pos


data Cursor = Cursor
  { nodeType :: !CString
  , nodeSymbol :: !Word16
  , nodeStartPoint :: !TSPoint
  , nodeEndPoint :: !TSPoint
  , nodeStartByte :: !Word32
  , nodeEndByte :: !Word32
  }
  deriving (Show, Eq, Generic)

data TSPoint = TSPoint { pointRow :: !Word32, pointColumn :: !Word32 }
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

instance Storable TSPoint where
  alignment _ = alignment (0 :: Int32)
  sizeOf _ = 8
  peek = evalStruct $ TSPoint <$> peekStruct
                              <*> peekStruct
  poke _ _ = error "Cant poke"



data Navigation = Down | Next | Up

data Helpers a m where
  HelpersM :: Monad m => 
                        { nodeLabel      :: a -> m String
                        , nodeFirstChild :: (a -> m (Maybe a))
                        , nodeNext       :: (a -> m (Maybe a))
                        , nodeParent     :: (a -> m (Maybe a))
                        }
                        -> Helpers a m

helpersIO :: Helpers (Ptr Cursor) IO
helpersIO = HelpersM
            { nodeLabel      = label
            , nodeFirstChild = firstChild
            , nodeNext       = next
            , nodeParent     = parent
            }

helpersID :: Helpers (Z.TreePos Z.Full String) Identity
helpersID = HelpersM
            { nodeLabel      = return . Z.label
            , nodeFirstChild = return . Z.firstChild
            , nodeNext       = return . Z.next
            , nodeParent     = return . Z.parent
            }

tsTransformTree :: Monad m => Helpers a m -> a -> m (Z.TreePos Z.Full String)
tsTransformTree helpers ptrCur = do
  rootLabel <- (nodeLabel helpers) ptrCur
  let resultZipper = Z.fromTree $ T.Node rootLabel []
   in go helpers resultZipper Down ptrCur
  where
    go :: Monad m => Helpers a m -> (Z.TreePos Z.Full String) -> Navigation -> a -> m (Z.TreePos Z.Full String)
    go helpers resultZipper nav ptrCur = case nav of
      Down -> do
        fc <- (nodeFirstChild helpers) ptrCur
        case fc of
          Nothing   -> go helpers resultZipper Next ptrCur
          Just node -> do
            l <- (nodeLabel helpers) node
            go helpers (Z.insert (T.Node l []) (Z.children resultZipper)) Down node
      Next -> do
        n <- (nodeNext helpers) ptrCur
        case n of
          Nothing   -> go helpers resultZipper Up ptrCur
          Just node -> do
            l <- (nodeLabel helpers) node
            go helpers (Z.insert (T.Node l []) (Z.nextSpace resultZipper)) Down node
      Up -> do
        p <- (nodeParent helpers) ptrCur
        case p of
          Nothing   -> return resultZipper
          Just node -> go helpers (fromJust $ Z.parent resultZipper) Next node



-- [SpanInfo]

data SpanInfo = Parent Span | Token Span
  deriving (Show, Eq, Ord)

spanInfoFromCursor :: Ptr Cursor -> IO SpanInfo
spanInfoFromCursor ptrCur = do
  span <- locspan ptrCur
  isParent <- hasChildren
  return $ case isParent of
    True -> Parent span
    False -> Token span

tsTransformList :: Ptr Cursor -> IO [SpanInfo]
tsTransformList ptrCur = do
  spanInfo <- spanInfoFromCursor ptrCur
  go [spanInfo] Down ptrCur
    where
      go :: [SpanInfo] -> Navigation -> Ptr Cursor -> IO [SpanInfo]
      go spanInfos nav ptrCur = case nav of
        Down -> do
          fc <- firstChild ptrCur
          case fc of
            Nothing      -> go spanInfos Next ptrCur
            Just ptrCur' -> do
              spanInfo <- spanInfoFromCursor ptrCur'
              go (spanInfo:spanInfos) Down ptrCur'
        Next -> do
          n <- next ptrCur
          case n of
            Nothing      -> go spanInfos Up ptrCur
            Just ptrCur' -> do
              spanInfo <- spanInfoFromCursor ptrCur'
              go (spanInfo:spanInfos) Down ptrCur'
        Up -> do
          p <- parent ptrCur
          case p of
            Nothing      -> return spanInfos
            Just ptrCur' -> go spanInfos Next ptrCur'


saveLine :: TSPoint -> Line
saveLine tsp =
  let row = pointRow tsp
      saveRow = row + 1
    in fromInteger $ toInteger saveRow

saveColumn :: TSPoint -> Column
saveColumn tsp =
  let col = pointColumn tsp
      saveCol = col + 1
    in fromInteger $ toInteger saveCol

locspan :: Ptr Cursor -> IO Span
locspan cur = do
  node <- peek cur
  let nodeStart = nodeStartPoint node
      startLoc = loc (saveLine nodeStart) (saveColumn nodeStart)
      nodeEnd   = nodeEndPoint node
      endLoc = loc (saveLine nodeEnd) (saveColumn nodeEnd)
    in do
      return $ fromTo startLoc endLoc


label :: Ptr Cursor -> IO String
label cur = do
  node <- peek cur
  nodeType <- peekCString (nodeType node)
  let nodeStart = nodeStartPoint node
      nodeEnd   = nodeEndPoint node
      startPoint = show (pointRow nodeStart, pointColumn nodeStart)
      endPoint = show (pointRow nodeEnd, pointColumn nodeEnd)
    in      
    return $ nodeType ++ " " ++ startPoint ++ "-" ++ endPoint

firstChild :: Ptr Cursor -> IO (Maybe (Ptr Cursor))
firstChild cur = do
  exists <- ts_cursor_goto_first_child cur
  return $ boolToMaybe cur exists

next :: Ptr Cursor -> IO (Maybe (Ptr Cursor))
next cur = do
  exists <- ts_cursor_goto_next_sibling cur
  return $ boolToMaybe cur exists

parent :: Ptr Cursor -> IO (Maybe (Ptr Cursor))
parent cur = do
  exists <- ts_cursor_goto_parent cur
  return $ boolToMaybe cur exists

hasChildren :: IO Bool
hasChildren = do
  cbool <- ts_cursor_has_children
  return $ toBool cbool

boolToMaybe :: Ptr Cursor -> CBool  -> Maybe (Ptr Cursor)
boolToMaybe cur exists =
  if exists == 1
    then Just cur
    else Nothing



foreign import ccall ts_cursor_init :: Ptr Tree -> Ptr Cursor -> IO ()
foreign import ccall ts_cursor_goto_first_child :: Ptr Cursor -> IO CBool
foreign import ccall ts_cursor_goto_next_sibling :: Ptr Cursor -> IO CBool
foreign import ccall ts_cursor_goto_parent :: Ptr Cursor -> IO CBool
foreign import ccall ts_cursor_has_children :: IO CBool
foreign import ccall "&ts_cursor_free" funptr_ts_cursor_free :: FunPtr (Ptr Cursor -> IO ())
