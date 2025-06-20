module Network.Transport.Kaisui.Type.FrameTypeSpec
  ( spec
  ) where

import Network.Transport.Kaisui.Type.FrameType
import RIO
import qualified RIO.List as L
import qualified RIO.Partial as RIO'
import Test.QuickCheck
import Test.Syd

spec :: Spec
spec = describe "FrameType" $ do
  describe "Eq instance" $ do
    it "should be reflexive"
      $ property
      $ \(frameType :: FrameType) -> frameType == frameType

    it "should be symmetric"
      $ property
      $ \(f1 :: FrameType) (f2 :: FrameType) ->
        (f1 == f2) == (f2 == f1)

    it "should handle all cases correctly" $ do
      ProtoBufFrame `shouldBe` ProtoBufFrame
      HeartbeatFrame `shouldBe` HeartbeatFrame
      ErrorFrame `shouldBe` ErrorFrame
      ProtoBufFrame `shouldNotBe` HeartbeatFrame
      ProtoBufFrame `shouldNotBe` ErrorFrame
      HeartbeatFrame `shouldNotBe` ErrorFrame

  describe "Ord instance" $ do
    it "should be transitive"
      $ property
      $ \(f1 :: FrameType) (f2 :: FrameType) (f3 :: FrameType) ->
        not (f1 <= f2 && f2 <= f3) || (f1 <= f3)

    it "should be antisymmetric"
      $ property
      $ \(f1 :: FrameType) (f2 :: FrameType) ->
        not (f1 <= f2 && f2 <= f1) || (f1 == f2)

    it "should have consistent ordering with Enum" $ do
      let frames = [ProtoBufFrame, HeartbeatFrame, ErrorFrame]
      L.sort frames `shouldBe` frames

  describe "Enum instance" $ do
    it "should convert to/from Int correctly" $ do
      fromEnum ProtoBufFrame `shouldBe` 0
      fromEnum HeartbeatFrame `shouldBe` 1
      fromEnum ErrorFrame `shouldBe` 2
      RIO'.toEnum 0 `shouldBe` ProtoBufFrame
      RIO'.toEnum 1 `shouldBe` HeartbeatFrame
      RIO'.toEnum 2 `shouldBe` ErrorFrame

    it "should roundtrip through toEnum/fromEnum"
      $ property
      $ \(frameType :: FrameType) ->
        RIO'.toEnum (fromEnum frameType) == frameType

    it "should have correct succ/pred behavior" $ do
      RIO'.succ ProtoBufFrame `shouldBe` HeartbeatFrame
      RIO'.succ HeartbeatFrame `shouldBe` ErrorFrame
      RIO'.pred HeartbeatFrame `shouldBe` ProtoBufFrame
      RIO'.pred ErrorFrame `shouldBe` HeartbeatFrame

  describe "Bounded instance" $ do
    it "should have correct bounds" $ do
      minBound `shouldBe` ProtoBufFrame
      maxBound `shouldBe` ErrorFrame

    it "should enumerate all values from minBound to maxBound" $ do
      [minBound .. maxBound] `shouldBe` [ProtoBufFrame, HeartbeatFrame, ErrorFrame]

  describe "Show instance" $ do
    it "should produce expected string representations" $ do
      show ProtoBufFrame `shouldBe` "ProtoBufFrame"
      show HeartbeatFrame `shouldBe` "HeartbeatFrame"
      show ErrorFrame `shouldBe` "ErrorFrame"

  describe "QuickCheck properties" $ do
    it "should generate all possible values" $ property $ do
      frames <- sample' (arbitrary :: Gen FrameType)
      let uniqueFrames = L.nub frames
      length uniqueFrames `shouldBe` 3

    it "should properly shrink values" $ do
      shrink ErrorFrame `shouldBe` [ProtoBufFrame, HeartbeatFrame]
      shrink HeartbeatFrame `shouldBe` [ProtoBufFrame]
      shrink ProtoBufFrame `shouldBe` []

-- | Arbitrary instance for property testing
instance Arbitrary FrameType where
  arbitrary = elements [minBound .. maxBound]

  shrink ErrorFrame = [ProtoBufFrame, HeartbeatFrame]
  shrink HeartbeatFrame = [ProtoBufFrame]
  shrink ProtoBufFrame = []
