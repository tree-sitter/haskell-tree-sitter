{-# LANGUAGE DisambiguateRecordFields, OverloadedStrings, TypeApplications #-}
module Main (main) where

import qualified System.Path as Path
import           Test.Tasty
import           TreeSitter.PHP
import qualified TreeSitter.PHP.AST as PHP
import           TreeSitter.Test.Helpers
import           TreeSitter.Unmarshal

main :: IO ()
main =
  =   readCorpusFiles (Path.relDir "tree-sitter-php/vendor/tree-sitter-php/corpus")
  >>= traverse (testCorpus parse)
  >>= defaultMain . tests
  where parse = parseByteString @PHP.Program @() tree_sitter_php

tests :: [TestTree] -> TestTree
tests = testGroup "tree-sitter-php corpus tests"
