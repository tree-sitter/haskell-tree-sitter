{-# LANGUAGE TemplateHaskell #-}
module Main
( main
) where

import Language.Haskell.TH.Cleanup
import TreeSitter.GenerateSyntax
import TreeSitter.JSON.Internal
import TreeSitter.JSON.AST.Internal

main :: IO ()
main = do
  putStrLn moduleHeader
  putStrLn $(simplifiedTH =<< astDeclarationsForLanguage tree_sitter_json [''StringContent] "/Users/rob/Developer/GitHub/haskell-tree-sitter/tree-sitter-json/vendor/tree-sitter-json/src/node-types.json")

moduleHeader :: String
moduleHeader = unlines
  [ "{-# LANGUAGE DataKinds #-}"
  , "{-# LANGUAGE DeriveAnyClass #-}"
  , "{-# LANGUAGE DeriveGeneric #-}"
  , "{-# LANGUAGE DeriveTraversable #-}"
  , "{-# LANGUAGE DuplicateRecordFields #-}"
  , "{-# LANGUAGE TemplateHaskell #-}"
  , "{-# LANGUAGE TypeApplications #-}"
  , "{-# LANGUAGE GeneralizedNewtypeDeriving #-}"
  , "{-# LANGUAGE DerivingStrategies #-}"
  , "module TreeSitter.JSON.AST"
  , "( module TreeSitter.JSON.AST"
  , ", module TreeSitter.JSON.AST.Internal"
  , ", (GHC.Generics.:+:)(..)"
  , ") where"
  , ""
  , "import qualified Data.Foldable"
  , "import qualified Data.Text.Internal.Text"
  , "import qualified Data.Traversable"
  , "import qualified GHC.Base"
  , "import qualified GHC.Classes"
  , "import qualified GHC.Generics"
  , "import qualified GHC.List"
  , "import qualified GHC.Real"
  , "import qualified GHC.Show"
  , "import Prelude hiding (String)"
  , "import TreeSitter.JSON.AST.Internal"
  , "import TreeSitter.Unmarshal"
  , ""
  ]
