{-# LANGUAGE TypeApplications #-}
module Main
( main) where

import           Control.Monad
import qualified Data.ByteString as B
import           Data.Foldable
import           Gauge
import           System.Exit (die)
import           System.Environment (die)
import           TreeSitter.Ruby
import qualified TreeSitter.Ruby.AST as Rb
import           TreeSitter.Unmarshal

main :: IO ()
main = args >>= defaultMain . map (bench <*> nfIO . parseFile)

parseFile :: FilePath -> IO ()
parseFile = either die pure <=< parseByteString @Py.Module @() tree_sitter_python <=< B.readFile . Path.toString
