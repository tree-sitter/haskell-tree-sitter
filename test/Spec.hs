import           Test.Hspec

import           Foreign
import           Foreign.C.Types
import           Foreign.Storable
import           Foreign.C.String


import           TreeSitter.Parser
import           TreeSitter.Tree
import           TreeSitter.Language
import           TreeSitter.Haskell
import           TreeSitter.Node
import           TreeSitter.TsPoint
import           TreeSitter.CursorApi.Cursor
import           TreeSitter.CursorApi.Types

import qualified Data.Tree                     as T
import qualified Data.Tree.Zipper              as Z
import           Data.Maybe
import           Control.Monad.Identity

import qualified Test.QuickCheck               as QC
import           Test.QuickCheck.Gen
import           Test.QuickCheck.Arbitrary
import           Test.QuickCheck.Instances.Containers

import           Data.List


filterParents :: SpanInfo -> Bool
filterParents (Parent _ _ _)  = False
filterParents (Token _ _ _)  = True


-- QC.quickCheck $ QC.mapSize ((*) 100) $ QC.withMaxSuccess 500 prop_traverses_all

prop_traverses_all :: T.Tree String -> QC.Property
prop_traverses_all tree =
  QC.label ("tree size " ++ show (length $ T.flatten tree)) $ 
  tree == Z.toTree (runIdentity (tsTransformIdentityZipper (Z.fromTree tree)))


main :: IO ()
main = hspec $ do

  describe "tsTransform produces depth-first [SpanInfo]" $ 
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

  describe "quickcheck tsTransform" $
    it "quickchecks traversing entire TSTree, building a Tree"
      $ QC.property $ QC.mapSize (100 *) $ QC.withMaxSuccess 200 prop_traverses_all

  describe "TSNode" $ do
    it "has the same size as its C counterpart"
      $          sizeOf (undefined :: TSNode)
      `shouldBe` fromIntegral sizeof_tsnode

    it "roundtrips correctly"
      $              with (TSNode 1 2 3 4 nullPtr nullPtr) peek
      `shouldReturn` TSNode 1 2 3 4 nullPtr nullPtr

  describe "TSPoint" $ do
    it "has the same size as its C counterpart"
      $          sizeOf (undefined :: TSPoint)
      `shouldBe` fromIntegral sizeof_tspoint

    it "roundtrips correctly" $ with (TSPoint 1 2) peek `shouldReturn` TSPoint
      1
      2

  describe "Node" $ do
    it "has the same size as its C counterpart"
      $          sizeOf (undefined :: Node)
      `shouldBe` fromIntegral sizeof_node

    it "roundtrips correctly"
      $              with
                       (Node (TSNode 1 2 3 4 nullPtr nullPtr)
                             nullPtr
                             1
                             (TSPoint 2 3)
                             (TSPoint 4 5)
                             6
                             7
                             8
                       )
                       peek
      `shouldReturn` Node (TSNode 1 2 3 4 nullPtr nullPtr)
                          nullPtr
                          1
                          (TSPoint 2 3)
                          (TSPoint 4 5)
                          6
                          7
                          8

foreign import ccall unsafe "src/bridge.c sizeof_tsnode" sizeof_tsnode :: CSize
foreign import ccall unsafe "src/bridge.c sizeof_tspoint" sizeof_tspoint :: CSize
foreign import ccall unsafe "src/bridge.c sizeof_node" sizeof_node :: CSize
