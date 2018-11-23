import Foreign
import Foreign.C.Types
import Foreign.Storable
import Test.Hspec

import           TreeSitter.Parser
import           TreeSitter.Tree
import           TreeSitter.Language
import           TreeSitter.Haskell
import           TreeSitter.Node
import           TreeSitter.Cursor

import           Foreign.ForeignPtr
import           Foreign.C
import           Foreign.C.String
import           Foreign.C.Types
import           Foreign.Ptr                    ( Ptr(..)
                                                , nullPtr
                                                )

import Data.List


filterParents :: SpanInfo -> Bool
filterParents (Parent _)  = False
filterParents (Token _)  = True


main :: IO ()
main = hspec $ do

  describe "tsTransform produces depth-first [SpanInfo]" $ do
    it "traverses entire TSTree, building a [SpanInfo]" $ do
      parser <- ts_parser_new
      ts_parser_set_language parser tree_sitter_haskell

      let source =
            "module Test (f1) where\nimport Lib\nf1 = f2 42\nf2 n = n + 1"
      (str, len) <- newCStringLen source

      tree       <- ts_parser_parse_string parser nullPtr str len

      fgnPtr     <- mallocForeignPtr :: IO (ForeignPtr Cursor)
      addForeignPtrFinalizer funptr_ts_cursor_free fgnPtr

      spaninfos <- withForeignPtr fgnPtr $ \cur -> do
        ts_cursor_init tree cur

        spanInfos <- tsTransformSpanInfos cur
        return $ reverse spanInfos

      let filteredSpaninfos = filter filterParents spaninfos
          sortedSpaninfos   = sort filteredSpaninfos
        in sortedSpaninfos `shouldBe` filteredSpaninfos