{-# LANGUAGE DisambiguateRecordFields, OverloadedStrings, TypeApplications #-}
module Main (main) where

import qualified System.Path as Path
import           Test.Tasty
import           TreeSitter.Python
import qualified TreeSitter.Python.AST as Py
import           TreeSitter.Test.Helpers
import           TreeSitter.Unmarshal

import qualified Manual.Examples

main :: IO ()
main
  =   readCorpusFiles (Path.relDir "./vendor/tree-sitter-python/corpus")
  >>= traverse (testCorpus parse)
  >>= defaultMain . tests
  where parse = parseByteString @Py.Module @() tree_sitter_python

tests :: [TestTree] -> TestTree
tests xs = testGroup "Tests"
  [ testGroup "tree-sitter corpus tests" xs
  , Manual.Examples.tests
  ]
