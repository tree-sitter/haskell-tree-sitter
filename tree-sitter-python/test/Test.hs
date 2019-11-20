{-# LANGUAGE TypeApplications, DisambiguateRecordFields, OverloadedStrings, TemplateHaskell #-}
module Main (main) where

import           Control.Applicative
import           Control.Monad
import           Data.Attoparsec.ByteString.Char8
import           Data.ByteString (ByteString, readFile)
import           Data.ByteString.Char8 (pack, unpack)
import           Data.Either
import           Data.Functor
import           Prelude hiding (takeWhile)
import           System.Exit (exitFailure)
import qualified System.Path as Path
import qualified System.Path.Directory as Path
import           Test.Tasty
import           Test.Tasty.HUnit
import           TreeSitter.Python
import qualified TreeSitter.Python.AST as Py
import           TreeSitter.Unmarshal

import qualified Manual.Examples

main :: IO ()
main = readCorpusFiles (Path.relDir "./vendor/tree-sitter-python/corpus") >>= traverse testCorpus >>= defaultMain . tests

tests :: [TestTree] -> TestTree
tests xs = testGroup "Tests"
  [ testGroup "tree-sitter corpus tests" xs
  , Manual.Examples.tests
  ]

testCorpus :: Path.RelFile -> IO TestTree
testCorpus path = do
  xs <- parseCorpusFile path
  case xs of
    Left e -> print ("Failed to parse corpus: " <> show (Path.toString path) <> " " <> "Error: " <> show e) *> exitFailure
    Right xs -> testGroup (Path.toString path) <$> traverse corpusTestCase xs
  where
    corpusTestCase (CorpusExample name code) = testCase name . either (errMsg code) pass <$> parse code
    parse = parseByteString @Py.Module @() tree_sitter_python
    pass = const (pure ())
    errMsg code e = assertFailure (e <> "\n``` python\n" <> unpack code <> "```")

-- Read and parse tree-sitter corpus examples

readCorpusFiles :: Path.RelDir ->  IO [Path.RelFile]
readCorpusFiles dir = fmap (Path.combine dir) <$> Path.filesInDir dir

data CorpusExample = CorpusExample { name :: String, code :: ByteString }
  deriving (Eq, Show)

parseCorpusFile :: Path.RelFile -> IO (Either String [CorpusExample])
parseCorpusFile path = do
  c <- Data.ByteString.readFile (Path.toString path)
  pure $ parseOnly corpusParser c

corpusParser :: Parser [CorpusExample]
corpusParser = do
  xs <- many' exampleParser
  void endOfInput
  pure xs

exampleParser :: Parser CorpusExample
exampleParser = do
  name <- exampleNameParser
  code <- manyTill anyChar (string "\n---\n")
  _out <- manyTill anyChar (choice [endOfInput, char '=' $> ()])
  pure (CorpusExample name (pack code))

exampleNameParser :: Parser String
exampleNameParser = do
  _ <- skipWhile (== '=') *> skipSpace
  name <- takeWhile (/= '\n')
  _ <- skipSpace *> skipWhile (== '=') *> skipSpace
  pure (unpack name)
