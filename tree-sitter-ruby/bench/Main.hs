module Main (main) where

import           Gauge
import qualified Parsing

main :: IO ()
-- main = defaultMain [Parsing.benchmarks]
main = Parsing.parseFile "../semantic/tmp/ruby-examples/ruby_spec/command_line/rubyopt_spec.rb"
