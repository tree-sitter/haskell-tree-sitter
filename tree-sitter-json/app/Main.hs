{-# LANGUAGE TemplateHaskell #-}
module Main
( main
) where

import Data.List (intersperse)
import Language.Haskell.TH
import TreeSitter.GenerateSyntax
import TreeSitter.JSON.Internal
import TreeSitter.JSON.AST.Internal

main :: IO ()
main = do
  let jsonPath = "tree-sitter-json/vendor/tree-sitter-json/src/node-types.json"
  ast <- runQ (astDeclarationsForLanguage tree_sitter_json [''StringContent] jsonPath)
  putStrLn moduleHeader
  putStrLn (unlines (intersperse "" (map pprint ast)))

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
  , "module Language.JSON.AST"
  , "( module Language.JSON.AST"
  , ", module TreeSitter.JSON.AST.Internal"
  , ", (:+:)(..)"
  , ") where"
  , ""
  , "import GHC.Generics ((:+:)(..))"
  , "import Prelude hiding (String)"
  , "import TreeSitter.JSON.AST.Internal"
  , "import TreeSitter.Unmarshal"
  , ""
  ]
