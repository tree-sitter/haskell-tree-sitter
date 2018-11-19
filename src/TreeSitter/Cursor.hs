{-# LANGUAGE DeriveGeneric, DeriveAnyClass, InterruptibleFFI, RankNTypes, ScopedTypeVariables, FlexibleInstances, MultiParamTypeClasses #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}
module TreeSitter.Cursor (
  Cursor(..)
  , readTreeSitter
  , traverseTreeSitter
  , ts_ptr_init  
  , ts_ptr_goto_first_child
  , ts_ptr_goto_next_sibling
  , ts_ptr_goto_parent
  , funptr_ts_ptr_free
) where

import Foreign
import Foreign.Ptr
import Foreign.C
import GHC.Generics

import TreeSitter.Tree
import TreeSitter.Struct

import qualified Data.Tree as T
import qualified Data.Tree.Zipper as Z
import Data.Maybe
import Control.Monad.Identity


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

class Monad m => TreeCursor m a where
  nodeLabel      :: a -> m String
  nodeFirstChild :: a -> m (Maybe a)
  nodeNext       :: a -> m (Maybe a)
  nodeParent     :: a -> m (Maybe a)

instance TreeCursor IO (Ptr Cursor) where
  nodeLabel      = label
  nodeFirstChild = firstChild
  nodeNext       = next
  nodeParent     = parent

instance TreeCursor Identity (Z.TreePos Z.Full String) where
  nodeLabel      = return . Z.label
  nodeFirstChild = return . Z.firstChild
  nodeNext       = return . Z.next
  nodeParent     = return . Z.parent


traverseTreeSitter :: TreeCursor m a => a -> m (T.Tree String)
traverseTreeSitter ptrCur = do
  rootLabel <- nodeLabel ptrCur
  let resultZipper = Z.fromTree $ T.Node rootLabel []
   in go resultZipper Down ptrCur
  where
    go :: TreeCursor m a => (Z.TreePos Z.Full String) -> Navigation -> a -> m (T.Tree String)
    go resultZipper nav ptrCur = case nav of
      Down -> do
        fc <- nodeFirstChild ptrCur
        case fc of
          Nothing   -> go resultZipper Next ptrCur
          Just node -> do
            l <- nodeLabel node
            go (Z.insert (T.Node l []) (Z.children resultZipper)) Down node
      Next -> do
        n <- nodeNext ptrCur
        case n of
          Nothing   -> go resultZipper Up ptrCur
          Just node -> do
            l <- nodeLabel node
            go (Z.insert (T.Node l []) (Z.nextSpace resultZipper)) Down node
      Up -> do
        p <- nodeParent ptrCur
        case p of
          Nothing   -> return $ Z.toTree resultZipper
          Just node -> go (fromJust $ Z.parent resultZipper) Next node

-- traverseTreeSitter :: Ptr Cursor -> IO (T.Tree String)
-- traverseTreeSitter ptrCur =
--   let resultZipper = Z.fromTree $ T.Node "ROOT" []
--    in go resultZipper Down ptrCur
--   where
--     go :: Z.TreePos Z.Full String -> Navigation -> Ptr Cursor -> IO (T.Tree String)
--     go resultZipper nav ptrCur = case nav of
--       Down -> do
--         fc <- firstChild ptrCur
--         case fc of
--           Nothing   -> go resultZipper Next ptrCur
--           Just node -> do
--             l <- label ptrCur
--             go (Z.insert (T.Node l []) (Z.children resultZipper)) Down node
--       Next -> do
--         n <- next ptrCur
--         case n of
--           Nothing   -> go resultZipper Up ptrCur
--           Just node -> do
--             l <- label ptrCur
--             go (Z.insert (T.Node l []) (Z.nextSpace resultZipper)) Down node
--       Up -> do
--         p <- parent ptrCur
--         case p of
--           Nothing   -> return $ Z.toTree resultZipper
--           Just node -> go (fromJust $ Z.parent resultZipper) Next node


readTreeSitter :: Ptr Cursor -> IO ()
readTreeSitter cur =
  go Down cur
  where
    go :: Navigation -> Ptr Cursor -> IO ()
    go nav z = case nav of
      Down -> do
        fc <- firstChild z
        case fc of
          Nothing   -> go Next z
          Just node -> do
            l <- label z
            print l >> go Down node
      Next -> do
        n <- next z
        case n of
          Nothing   -> go Up z
          Just node -> do
            l <- label z
            print l >> go Down node
      Up -> do
        p <- parent z
        case p of
          Nothing   -> print "END"
          Just node -> go Next node


label :: Ptr Cursor -> IO String
label cur = do
  node <- peek cur
  typeAsString <- peekCString (nodeType node)
  return typeAsString

firstChild :: Ptr Cursor -> IO (Maybe (Ptr Cursor))
firstChild cur = do
  exists <- ts_ptr_goto_first_child cur
  return $ boolToMaybe cur exists

next :: Ptr Cursor -> IO (Maybe (Ptr Cursor))
next cur = do
  exists <- ts_ptr_goto_next_sibling cur
  return $ boolToMaybe cur exists

parent :: Ptr Cursor -> IO (Maybe (Ptr Cursor))
parent cur = do
  exists <- ts_ptr_goto_parent cur
  return $ boolToMaybe cur exists

boolToMaybe :: Ptr Cursor -> CBool  -> Maybe (Ptr Cursor)
boolToMaybe cur exists =
  if exists == 1
    then Just cur
    else Nothing



foreign import ccall ts_ptr_init :: Ptr Tree -> Ptr Cursor -> IO ()
foreign import ccall ts_ptr_goto_first_child :: Ptr Cursor -> IO CBool
foreign import ccall ts_ptr_goto_next_sibling :: Ptr Cursor -> IO CBool
foreign import ccall ts_ptr_goto_parent :: Ptr Cursor -> IO CBool
foreign import ccall "&ts_ptr_free" funptr_ts_ptr_free :: FunPtr (Ptr Cursor -> IO ())
