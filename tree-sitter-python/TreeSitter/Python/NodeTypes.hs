{-# LANGUAGE TemplateHaskell #-}

module TreeSitter.Python.NodeTypes (nodeTypes) where

import Data.FileEmbed
import Data.ByteString (ByteString)

nodeTypes :: ByteString
nodeTypes = $(embedFile "vendor/tree-sitter-python/src/node-types.json")
