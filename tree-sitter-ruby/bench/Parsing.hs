{-# LANGUAGE TypeApplications         #-}
{-# LANGUAGE FlexibleContexts         #-}

module Parsing
  ( benchmarks
  , parseFile
  -- , parseFile'
  ) where


import           Control.Carrier.Profile.Tree
import           Control.Monad
import           Control.Monad.IO.Class
import qualified Data.ByteString as B
import           Data.Foldable
import           Gauge
import           System.Exit (die)
import           System.FilePath.Glob
import qualified System.Path as Path
import           TreeSitter.Ruby
import qualified TreeSitter.Ruby.AST as Rb
import           TreeSitter.Unmarshal

benchmarks :: Benchmark
benchmarks = bgroup "parsing" [ rubyBenchmarks ]

rubyBenchmarks :: Benchmark
rubyBenchmarks = bench "ruby" $ parseAllFiles dir "*.rb"
  where dir = Path.relDir "../semantic/tmp/ruby-examples/ruby_spec/command_line"

-- parseAllFiles' :: Path.RelDir -> String -> Benchmarkable
-- parseAllFiles' dir glob = nfIO . reportProfile $ do
--   files <- liftIO $ globDir1 (compile glob) (Path.toString dir)
--   let paths = Path.relFile <$> files
--   when (null paths) (liftIO . die $ "No files found in " <> (Path.toString dir))
--   for_ paths $ \ file -> do
--     contents <- liftIO $ B.readFile (Path.toString file)
--     either (liftIO . die) pure =<< parseByteString' @Rb.Program @() tree_sitter_ruby contents

parseAllFiles :: Path.RelDir -> String -> Benchmarkable
parseAllFiles dir glob = nfIO $ do
  files <- liftIO $ globDir1 (compile glob) (Path.toString dir)
  let paths = Path.relFile <$> files
  when (null paths) (die $ "No files found in " <> (Path.toString dir))
  for_ paths $ \ file -> do
    contents <- B.readFile (Path.toString file)
    either die pure =<< parseByteString @Rb.Program @() tree_sitter_ruby contents

parseFile :: FilePath -> IO ()
parseFile file = do
  contents <- B.readFile file
  either die (\ _ -> pure ()) =<< parseByteString @Rb.Program @() tree_sitter_ruby contents

-- -- parseFile' :: FilePath -> IO ()
-- parseFile' file = reportProfile $ do
--   contents <- liftIO $ B.readFile file
--   either (liftIO . die) (\ _ -> pure ()) =<< parseByteString' @Rb.Program @() tree_sitter_ruby contents
