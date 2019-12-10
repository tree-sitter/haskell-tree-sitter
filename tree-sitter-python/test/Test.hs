{-# LANGUAGE DisambiguateRecordFields, OverloadedStrings, TypeApplications #-}
module Main (main) where

import           Control.Monad
import           System.Path ((</>))
import qualified System.Path as Path
import           System.Path.Directory
import           Test.Tasty
import           TreeSitter.Python
import qualified TreeSitter.Python.AST as Py
import           TreeSitter.Test.Helpers
import           TreeSitter.Unmarshal

main :: IO ()
main = do
  dir <- getCurrentDirectory
  when (Path.takeDirName dir == Just (Path.relDir "haskell-tree-sitter")) $
    setCurrentDirectory (dir </> Path.relDir "tree-sitter-python")
  let parse = parseByteString @Py.Module @() tree_sitter_python
  readCorpusFiles (Path.relDir "./vendor/tree-sitter-python/corpus")
    >>= traverse (testCorpus parse)
    >>= defaultMain . tests
  where

tests :: [TestTree] -> TestTree
tests = testGroup "tree-sitter-python corpus tests"
