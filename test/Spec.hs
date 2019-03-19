import Foreign
import Foreign.C.Types
import Foreign.Storable
import Test.Hspec
import TreeSitter.Node
import TreeSitter.Parser

main :: IO ()
main = hspec $ do
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

  describe "Parser" $ do
    it "stores a timeout value" $ do
      parser <- ts_parser_new
      timeout <- ts_parser_timeout_micros parser
      timeout `shouldBe` 0
      ts_parser_set_timeout_micros parser 1000
      timeout <- ts_parser_timeout_micros parser
      timeout `shouldBe` 1000
      ts_parser_delete parser

foreign import ccall unsafe "src/bridge.c sizeof_tsnode" sizeof_tsnode :: CSize
foreign import ccall unsafe "src/bridge.c sizeof_tspoint" sizeof_tspoint :: CSize
foreign import ccall unsafe "src/bridge.c sizeof_node" sizeof_node :: CSize
