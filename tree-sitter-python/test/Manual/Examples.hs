{-# LANGUAGE DisambiguateRecordFields, OverloadedStrings, OverloadedLists, TemplateHaskell #-}
module Manual.Examples (tests) where

import           Control.Monad.IO.Class
import           Data.ByteString (ByteString)
import           Data.List.NonEmpty
import           GHC.Generics
import           Hedgehog
import           Hedgehog.Internal.Property (PropertyName (..))
import           Test.Tasty
import           Test.Tasty.Hedgehog as Hedgehog
import           TreeSitter.Python
import qualified TreeSitter.Python.AST as Py
import           TreeSitter.Token
import           TreeSitter.Unmarshal

tests :: TestTree
tests = testGroup "(checked by Hedgehog)" props
  where
    props = let (Group _ xs) = $$(discover)
            in fmap toP xs
    toP ((PropertyName name), prop) = Hedgehog.testProperty name prop


-- TODO: add tests that verify correctness for product, sum and leaf types

shouldParseInto :: (MonadIO m, MonadTest m, Unmarshal t, UnmarshalAnn a, Eq (t a), Show (t a)) => ByteString -> t a -> m ()
s `shouldParseInto` t = do
  parsed <- liftIO $ parseByteString tree_sitter_python s
  parsed === Right t

pass = Py.SimpleStatement (R1 (R1 (L1 (L1 (Py.PassStatement () "pass")))))
one = Py.SimpleStatement (L1 (R1 (R1 (L1 (Py.ExpressionStatement () [L1 (L1 (Py.Expression (L1 (L1 (L1 (Py.PrimaryExpression (R1 (L1 (L1 (L1 (Py.Integer () "1")))))))))))])))))
plusOne = Py.SimpleStatement (L1 (R1 (R1 (L1 (Py.ExpressionStatement () [L1 (L1 (Py.Expression (L1 (L1 (L1 (Py.PrimaryExpression (R1 (R1 (R1 (R1 (R1 (Py.UnaryOperator () (L1 (Token ())) (Py.PrimaryExpression (R1 (L1 (L1 (L1 (Py.Integer () "1")))))) ))))))))))))])))))
identifier = Py.SimpleStatement (L1 (R1 (R1 (L1 (Py.ExpressionStatement () [L1 (L1 (Py.Expression (L1 (L1 (L1 (Py.PrimaryExpression (L1 (R1 (R1 (R1 (R1 (Py.Identifier () "hello"))))))))))))])))))
fromImport = Py.SimpleStatement (R1 (L1 (L1 (R1 importStatement))))
importStatement = Py.ImportFromStatement
                    ()
                    [ R1 (Py.DottedName () (Py.Identifier () "a" :| []))
                    , R1 (Py.DottedName () (Py.Identifier () "b" :| []))
                    ]
                    (L1 (Py.DottedName () (Py.Identifier () "foo" :| [])))
                    Nothing

prop_simpleExamples :: Property
prop_simpleExamples = property $ do
  "" `shouldParseInto` Py.Module { Py.ann = (), Py.extraChildren = [] }
  "# bah" `shouldParseInto` Py.Module { Py.ann = (), Py.extraChildren = [] }
  "pass" `shouldParseInto` Py.Module { Py.ann = (), Py.extraChildren = [R1 pass] }
  "1" `shouldParseInto` Py.Module { Py.ann = (), Py.extraChildren = [R1 one] }
  "+1" `shouldParseInto` Py.Module { Py.ann = (), Py.extraChildren = [R1 plusOne] }
  "hello" `shouldParseInto` Py.Module { Py.ann = (), Py.extraChildren = [R1 identifier] }
  "1\npass" `shouldParseInto` Py.Module { Py.ann = (), Py.extraChildren = [R1 one, R1 pass] }
  "from foo import a, b" `shouldParseInto` Py.Module { Py.ann = (), Py.extraChildren = [R1 fromImport] }
