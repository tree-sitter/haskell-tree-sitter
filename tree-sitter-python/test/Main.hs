{-# LANGUAGE DisambiguateRecordFields, OverloadedStrings, OverloadedLists, TemplateHaskell #-}

module Main where

import           TreeSitter.GenerateSyntax
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Bool (bool)
import           Data.ByteString (ByteString)
import           Data.Char
import           Data.Foldable
import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import           System.Exit (exitFailure, exitSuccess)
import           TreeSitter.Python
import qualified TreeSitter.Python.AST as Py
import           TreeSitter.Unmarshal

-- TODO: add tests that verify correctness for product, sum and leaf types

shouldParseInto :: (MonadIO m, MonadTest m, Unmarshal t, Eq t, Show t) => ByteString -> t -> m ()
s `shouldParseInto` t = do
  parsed <- liftIO $ parseByteString tree_sitter_python s
  parsed === Right t

pass = Py.PassStatementSimpleStatement (Py.PassStatement () "pass" )
one = Py.ExpressionStatementSimpleStatement (Py.ExpressionStatement () [Left (Py.PrimaryExpressionExpression (Py.IntegerPrimaryExpression (Py.Integer () "1")))])
function = Py.ExpressionStatementSimpleStatement (Py.ExpressionStatement () [Left (Py.PrimaryExpressionExpression (Py.IdentifierPrimaryExpression (Py.Identifier () "expensive")))])

prop_simpleExamples :: Property
prop_simpleExamples = property $ do
  "pass" `shouldParseInto` Py.Module { Py.ann = (), Py.extraChildren = [Right pass] }
  "1" `shouldParseInto` Py.Module { Py.ann = (), Py.extraChildren = [Right one] }
  "expensive" `shouldParseInto` Py.Module { Py.ann = (), Py.extraChildren = [Right function] }

main = checkParallel $$(discover) >>= bool exitFailure exitSuccess
