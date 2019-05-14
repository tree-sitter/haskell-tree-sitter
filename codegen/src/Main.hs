module Main where

import Language.Haskell.TH
import CodeGen.Deserialize
import CodeGen.GenerateSyntax
import Control.Monad.IO.Class
import Language.Haskell.TH
import Data.Aeson as Aeson

pythonPath :: String
pythonPath = "languages/python/vendor/tree-sitter-python/src/node-types.json"

-- Read JSON as input
input :: Q [MkDatatype]
input = liftIO (eitherDecodeFileStrict' pythonPath) >>= either fail pure

main :: IO ()
main = do
  result <- runQ (input >>= traverse datatypeForConstructors)
  putStrLn "codegen test"
  putStrLn $ pprint result
