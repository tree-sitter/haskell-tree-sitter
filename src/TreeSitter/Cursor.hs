{-# LANGUAGE DeriveGeneric, DeriveAnyClass, InterruptibleFFI, RankNTypes, ScopedTypeVariables #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}
module TreeSitter.Cursor (
  Cursor(..)
  , readTreeSitter
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
