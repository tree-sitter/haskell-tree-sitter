module TreeSitter.Cursor where

import TreeSitter.Ptr

import           Foreign.Ptr                    
import           Foreign.C

type Cursor = Ptr TreeSitter_Ptr

data Navigation = Down | Next | Up


readTreeSitter :: Cursor -> IO ()
readTreeSitter cur =
  go Down cur
  where
    go :: Navigation -> Cursor -> IO ()
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


label :: Cursor -> IO String
label cur = do
  nodeType <- ts_ptr_current_type cur
  typeAsString <- peekCString nodeType
  return typeAsString

firstChild :: Cursor -> IO (Maybe Cursor)
firstChild cur = do
  exists <- ts_ptr_goto_first_child cur
  return $ boolToMaybe cur exists

next :: Cursor -> IO (Maybe Cursor)
next cur = do
  exists <- ts_ptr_goto_next_sibling cur
  return $ boolToMaybe cur exists

parent :: Cursor -> IO (Maybe Cursor)
parent cur = do
  exists <- ts_ptr_goto_parent cur
  return $ boolToMaybe cur exists

boolToMaybe :: Cursor -> CBool  -> Maybe Cursor
boolToMaybe cur exists =
  if exists == 1
    then Just cur
    else Nothing

