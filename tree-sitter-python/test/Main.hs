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
import           GHC.Generics

-- TODO: add tests that verify correctness for product, sum and leaf types

shouldParseInto :: (MonadIO m, MonadTest m, Unmarshal t, Eq t, Show t) => ByteString -> t -> m ()
s `shouldParseInto` t = do
  parsed <- liftIO $ parseByteString tree_sitter_python s
  parsed === Right t

pass = Py.PassStatementSimpleStatement (Py.PassStatement () "pass" )
one = Py.ExpressionStatementSimpleStatement (Py.ExpressionStatement () [L1 (Py.PrimaryExpressionExpression (Py.IntegerPrimaryExpression (Py.Integer () "1")))])
function = Py.ExpressionStatementSimpleStatement (Py.ExpressionStatement () [L1 (Py.PrimaryExpressionExpression (Py.IdentifierPrimaryExpression (Py.Identifier () "expensive")))])

prop_simpleExamples :: Property
prop_simpleExamples = property $ do
  "" `shouldParseInto` Py.Module { Py.ann = (), Py.extraChildren = [] }
  "# bah" `shouldParseInto` Py.Module { Py.ann = (), Py.extraChildren = [] }
  "pass" `shouldParseInto` Py.Module { Py.ann = (), Py.extraChildren = [R1 pass] }
  "1" `shouldParseInto` Py.Module { Py.ann = (), Py.extraChildren = [R1 one] }
  "expensive" `shouldParseInto` Py.Module { Py.ann = (), Py.extraChildren = [R1 function] }
  "1\npass" `shouldParseInto` Py.Module { Py.ann = (), Py.extraChildren = [R1 one, R1 pass] }



main = checkParallel $$(discover) >>= bool exitFailure exitSuccess
