{-# LANGUAGE TypeApplications, DisambiguateRecordFields, OverloadedStrings, TemplateHaskell #-}
module Main (main) where

import           Control.Applicative
import           Control.Monad
import           Data.Attoparsec.ByteString.Char8
import           Data.ByteString (ByteString, readFile)
import           Data.ByteString.Char8 (pack, unpack)
import           Data.Either
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
main = corpusFiles >>= traverse testCorpus >>= defaultMain . tests

tests :: [TestTree] -> TestTree
tests xs = testGroup "Tests"
  [ testGroup "tree-sitter corpus tests" xs
  , Manual.Examples.tests
  ]

data CorpusExample = CorpusExample { name :: String, code :: ByteString }
  deriving (Eq, Show)

corpusFiles :: IO [Path.RelFile]
corpusFiles = let dir = Path.relDir "./vendor/tree-sitter-python/corpus"
              in fmap (Path.combine dir) <$> Path.filesInDir dir

testCorpus :: Path.RelFile -> IO TestTree
testCorpus path = do
  xs <- parseCorpusFile path
  case xs of
    Left e -> print ("failed to parse corpus: " <> show path) *> print e *> exitFailure
    Right xs -> (traverse toPropTest xs) >>= pure . testGroup (Path.toString path)
  where
    toPropTest (CorpusExample name code) = do
      res <- parseByteString @Py.Module @() tree_sitter_python code
      let failureMsg = "failed to parse:\n``` python\n" <> code <> "```"
      pure $ testCase name (assertBool (unpack failureMsg) (isRight res))

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
  _out <- manyTill anyChar (choice [endOfInput, char '=' *> pure ()])
  pure (CorpusExample name (pack code))

exampleNameParser :: Parser String
exampleNameParser = do
  _ <- skipWhile (== '=') *> skipSpace
  name <- takeWhile (/= '\n')
  _ <- skipSpace *> skipWhile (== '=') *> skipSpace
  pure (unpack name)
