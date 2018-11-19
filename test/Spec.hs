import Foreign
import Foreign.C.Types
import Foreign.Storable
import Test.Hspec
import TreeSitter.Node
import TreeSitter.Cursor

import qualified Data.Tree as T
import qualified Data.Tree.Zipper as Z
import Data.Maybe
import Control.Monad.Identity

import qualified Test.QuickCheck               as QC
import           Test.QuickCheck.Gen
import           Test.QuickCheck.Arbitrary
import           Test.QuickCheck.Instances.Containers


_prop_traverses_all :: T.Tree String -> QC.Property
_prop_traverses_all tree =
  QC.label ("tree size " ++ show (length $ T.flatten tree))
    $  tree
    == runIdentity (traverseTreeSitter (Z.fromTree tree))


main :: IO ()
main = hspec $ do
  describe "traverseTreeSitter" $ do
    it "traverses entire TSTree" $
      let tree = T.Node "1" [T.Node "2a" [], T.Node "2b" []]
          z    = Z.fromTree $ tree
      in 
        (runIdentity $ traverseTreeSitter z) `shouldBe` tree

  describe "quickcheck traverseTreeSitter" $ do
    it "quickchecks traversing entire TSTree" $
      -- QC.quickCheck $ QC.mapSize ((*) 100) $ QC.withMaxSuccess 500 _prop_traverses_all
      QC.property _prop_traverses_all

  describe "TSNode" $ do
    it "has the same size as its C counterpart" $
      sizeOf (undefined :: TSNode) `shouldBe` fromIntegral sizeof_tsnode

    it "roundtrips correctly" $
      with (TSNode 1 2 3 4 nullPtr nullPtr) peek `shouldReturn` TSNode 1 2 3 4 nullPtr nullPtr

  describe "TSPoint" $ do
    it "has the same size as its C counterpart" $
      sizeOf (undefined :: TSPoint) `shouldBe` fromIntegral sizeof_tspoint

    it "roundtrips correctly" $
      with (TSPoint 1 2) peek `shouldReturn` TSPoint 1 2

  describe "Node" $ do
    it "has the same size as its C counterpart" $
      sizeOf (undefined :: Node) `shouldBe` fromIntegral sizeof_node

    it "roundtrips correctly" $
      with (Node (TSNode 1 2 3 4 nullPtr nullPtr) nullPtr 1 (TSPoint 2 3) (TSPoint 4 5) 6 7 8) peek `shouldReturn` Node (TSNode 1 2 3 4 nullPtr nullPtr) nullPtr 1 (TSPoint 2 3) (TSPoint 4 5) 6 7 8

foreign import ccall unsafe "src/bridge.c sizeof_tsnode" sizeof_tsnode :: CSize
foreign import ccall unsafe "src/bridge.c sizeof_tspoint" sizeof_tspoint :: CSize
foreign import ccall unsafe "src/bridge.c sizeof_node" sizeof_node :: CSize
