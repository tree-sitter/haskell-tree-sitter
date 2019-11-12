module Main (main) where

import Control.Monad
import System.Exit (exitFailure)
import System.IO (BufferMode (..), hSetBuffering, stderr, stdout)

import qualified TreeSitter.Example
import qualified TreeSitter.Strings.Example

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering

  results <- sequence [
      TreeSitter.Example.tests
    , TreeSitter.Strings.Example.tests
    ]

  unless (and results) exitFailure
