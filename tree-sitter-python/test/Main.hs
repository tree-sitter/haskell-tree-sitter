{-# LANGUAGE DisambiguateRecordFields, OverloadedStrings, OverloadedLists, TemplateHaskell #-}
module Main (main) where

import           Control.Monad.IO.Class
import           Data.Bool (bool)
import           Data.ByteString (ByteString)
import           GHC.Generics
import           Hedgehog
import           System.Exit (exitFailure, exitSuccess)
import           TreeSitter.Python
import qualified TreeSitter.Python.AST as Py
import           TreeSitter.Token
import           TreeSitter.Unmarshal

-- TODO: add tests that verify correctness for product, sum and leaf types

shouldParseInto :: (MonadIO m, MonadTest m, Unmarshal t, UnmarshalAnn a, Eq (t a), Show (t a)) => ByteString -> t a -> m ()
s `shouldParseInto` t = do
  parsed <- liftIO $ parseByteString tree_sitter_python s
  parsed === Right t

pass = Py.SimpleStatement (R1 (R1 (L1 (L1 (Py.PassStatement () "pass")))))
one = Py.SimpleStatement (L1 (R1 (R1 (L1 (Py.ExpressionStatement () [L1 (L1 (Py.Expression (L1 (L1 (L1 (Py.PrimaryExpression (R1 (L1 (L1 (L1 (Py.Integer () "1")))))))))))])))))
plusOne = Py.SimpleStatement (L1 (R1 (R1 (L1 (Py.ExpressionStatement () [L1 (L1 (Py.Expression (L1 (L1 (L1 (Py.PrimaryExpression (R1 (R1 (R1 (R1 (R1 (Py.UnaryOperator () (L1 (Token ())) (Py.PrimaryExpression (R1 (L1 (L1 (L1 (Py.Integer () "1")))))) ))))))))))))])))))
identifier = Py.SimpleStatement (L1 (R1 (R1 (L1 (Py.ExpressionStatement () [L1 (L1 (Py.Expression (L1 (L1 (L1 (Py.PrimaryExpression (L1 (R1 (R1 (R1 (R1 (Py.Identifier () "hello"))))))))))))])))))


prop_simpleExamples :: Property
prop_simpleExamples = property $ do
  "" `shouldParseInto` Py.Module { Py.ann = (), Py.extraChildren = [] }
  "# bah" `shouldParseInto` Py.Module { Py.ann = (), Py.extraChildren = [] }
  "pass" `shouldParseInto` Py.Module { Py.ann = (), Py.extraChildren = [R1 pass] }
  "1" `shouldParseInto` Py.Module { Py.ann = (), Py.extraChildren = [R1 one] }
  "+1" `shouldParseInto` Py.Module { Py.ann = (), Py.extraChildren = [R1 plusOne] }
  "hello" `shouldParseInto` Py.Module { Py.ann = (), Py.extraChildren = [R1 identifier] }
  -- "1\npass" `shouldParseInto` Py.Module { Py.ann = (), Py.extraChildren = [R1 one, R1 pass] }

main :: IO ()
main = checkParallel $$(discover) >>= bool exitFailure exitSuccess

-- Right (Module {ann = Range {start = 0, end = 2}, extraChildren = [R1 (SimpleStatement (L1 (R1 (R1 (L1 (ExpressionStatement {ann = Range {start = 0, end = 2}, extraChildren = L1 (L1 (Expression (L1 (L1 (L1 (PrimaryExpression (R1 (R1 (R1 (R1 (R1 (UnaryOperator {ann = Range {start = 0, end = 2}, operator = L1 (Token {ann = Range {start = 0, end = 1}}), argument = PrimaryExpression (R1 (L1 (L1 (L1 (Integer {ann = Range {start = 1, end = 2}, bytes = "1"})))))})))))))))))) :| []}))))))]})

-- SimpleStatement (L1 (R1 (R1 (L1 (ExpressionStatement {ann = Range {start = 0, end = 2}, extraChildren = L1 (L1 (Expression (L1 (L1 (L1 (PrimaryExpression (R1 (R1 (R1 (R1 (R1 (UnaryOperator {ann = Range {start = 0, end = 2}, operator = L1 (Token {ann = Range {start = 0, end = 1}}), argument = PrimaryExpression (R1 (L1 (L1 (L1 (Integer {ann = Range {start = 1, end = 2}, bytes = "1"})))))})))))))))))) :| []})))))


-- Right (Module {ann = Range {start = 0, end = 5}, extraChildren = [R1 (SimpleStatement (L1 (R1 (R1 (L1 (ExpressionStatement {ann = Range {start = 0, end = 5}, extraChildren = L1 (L1 (Expression (L1 (L1 (L1 (PrimaryExpression (L1 (R1 (R1 (R1 (R1 (Identifier {ann = Range {start = 0, end = 5}, bytes = "hello"})))))))))))) :| []}))))))]})


-- {ann = Range {start = 0, end = 5}, extraChildren = L1 (L1 (Expression (L1 (L1 (L1 (PrimaryExpression (L1 (R1 (R1 (R1 (R1 (Identifier {ann = Range {start = 0, end = 5}, bytes = "hello"})))))))))))) :| []})))))
