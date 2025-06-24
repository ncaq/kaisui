module Network.Transport.Kaisui.Type.FrameHeaderSpec
  ( spec
  ) where

import Network.Transport.Kaisui.Type.FrameHeader
import Network.Transport.Kaisui.Type.FrameType
import Network.Transport.Kaisui.Type.FrameTypeSpec ()
import RIO
import qualified RIO.List as L
import Test.QuickCheck
import Test.Syd

spec :: Spec
spec = describe "FrameHeader" $ do
  describe "Eq instance" $ do
    it "should be reflexive"
      $ property
      $ \(header :: FrameHeader) -> header == header

    it "should be symmetric"
      $ property
      $ \(h1 :: FrameHeader) (h2 :: FrameHeader) ->
        (h1 == h2) == (h2 == h1)

    it "should compare fields correctly" $ do
      let header1 = FrameHeader ProtoBufFrame 100
          header2 = FrameHeader ProtoBufFrame 100
          header3 = FrameHeader HeartbeatFrame 100
          header4 = FrameHeader ProtoBufFrame 200
      header1 `shouldBe` header2
      header1 `shouldNotBe` header3
      header1 `shouldNotBe` header4

  describe "Ord instance" $ do
    it "should be transitive"
      $ property
      $ \(h1 :: FrameHeader) (h2 :: FrameHeader) (h3 :: FrameHeader) ->
        not (h1 <= h2 && h2 <= h3) || (h1 <= h3)

    it "should order by frameType first, then frameLength" $ do
      let headers =
            [ FrameHeader ErrorFrame 300
            , FrameHeader ProtoBufFrame 100
            , FrameHeader HeartbeatFrame 200
            , FrameHeader ProtoBufFrame 50
            ]
      L.sort headers
        `shouldBe` [ FrameHeader ProtoBufFrame 50
                   , FrameHeader ProtoBufFrame 100
                   , FrameHeader HeartbeatFrame 200
                   , FrameHeader ErrorFrame 300
                   ]

  describe "Show instance" $ do
    it "should produce expected string representations" $ do
      show (FrameHeader ProtoBufFrame 42) `shouldBe` "FrameHeader {frameType = ProtoBufFrame, frameLength = 42}"

  describe "Lens accessors" $ do
    it "should read frameType correctly"
      $ property
      $ \(ft :: FrameType) (len :: Word32) ->
        let header = FrameHeader ft len
         in header ^. frameType `shouldBe` ft

    it "should read frameLength correctly"
      $ property
      $ \(ft :: FrameType) (len :: Word32) ->
        let header = FrameHeader ft len
         in header ^. frameLength `shouldBe` len

    it "should modify frameType correctly"
      $ property
      $ \(header :: FrameHeader) (newType :: FrameType) ->
        let modified = header & frameType .~ newType
         in modified ^. frameType `shouldBe` newType

    it "should modify frameLength correctly"
      $ property
      $ \(header :: FrameHeader) (newLen :: Word32) ->
        let modified = header & frameLength .~ newLen
         in modified ^. frameLength `shouldBe` newLen

    it "should compose modifications correctly" $ do
      let original = FrameHeader ProtoBufFrame 100
          modified =
            original
              & frameType
              .~ HeartbeatFrame
                & frameLength
              .~ 200
      modified `shouldBe` FrameHeader HeartbeatFrame 200

  describe "QuickCheck properties" $ do
    it "should generate valid headers"
      $ property
      $ \(header :: FrameHeader) ->
        header ^. frameLength >= 0

    it "should shrink to simpler values" $ do
      let header = FrameHeader ErrorFrame 1000
          shrunken = shrink header
      shrunken `shouldContain` [FrameHeader ProtoBufFrame 1000]
      shrunken `shouldContain` [FrameHeader ErrorFrame 0]
      shrunken `shouldContain` [FrameHeader ErrorFrame 500]

  describe "HasFrameType typeclass" $ do
    it "should work with any type that has frameType" $ do
      let header = FrameHeader ProtoBufFrame 100
      header ^. frameType `shouldBe` ProtoBufFrame

  describe "HasFrameLength typeclass" $ do
    it "should work with any type that has frameLength" $ do
      let header = FrameHeader ProtoBufFrame 100
      header ^. frameLength `shouldBe` 100

-- | Arbitrary instance for property testing
instance Arbitrary FrameHeader where
  arbitrary =
    FrameHeader
      <$> arbitrary
      <*> arbitrary

  shrink (FrameHeader ft len) =
    [FrameHeader ft' len | ft' <- shrink ft]
      ++ [FrameHeader ft len' | len' <- shrink len]
