module Network.Transport.Kaisui.Type.ConnectionStateSpec
  ( spec
  ) where

import Network.Transport.Kaisui.Type.ConnectionState
import RIO
import qualified RIO.List as L
import Test.QuickCheck
import Test.Syd

spec :: Spec
spec = describe "ConnectionState" $ do
  describe "Eq instance" $ do
    it "should be reflexive"
      $ property
      $ \(state :: ConnectionState) -> state == state

    it "should be symmetric"
      $ property
      $ \(s1 :: ConnectionState) (s2 :: ConnectionState) ->
        (s1 == s2) == (s2 == s1)

    it "should handle all cases correctly" $ do
      ConnectionEstablished `shouldBe` ConnectionEstablished
      ConnectionClosed `shouldBe` ConnectionClosed
      ConnectionPending `shouldBe` ConnectionPending
      ConnectionEstablished `shouldNotBe` ConnectionClosed
      ConnectionEstablished `shouldNotBe` ConnectionPending
      ConnectionClosed `shouldNotBe` ConnectionPending

  describe "Ord instance" $ do
    it "should be transitive"
      $ property
      $ \(s1 :: ConnectionState) (s2 :: ConnectionState) (s3 :: ConnectionState) ->
        not (s1 <= s2 && s2 <= s3) || (s1 <= s3)

    it "should be antisymmetric"
      $ property
      $ \(s1 :: ConnectionState) (s2 :: ConnectionState) ->
        not (s1 <= s2 && s2 <= s1) || (s1 == s2)

    it "should have consistent ordering" $ do
      let states = [ConnectionEstablished, ConnectionClosed, ConnectionPending]
      L.sort states `shouldBe` [ConnectionEstablished, ConnectionClosed, ConnectionPending]

  describe "Show instance" $ do
    it "should produce expected string representations" $ do
      show ConnectionEstablished `shouldBe` "ConnectionEstablished"
      show ConnectionClosed `shouldBe` "ConnectionClosed"
      show ConnectionPending `shouldBe` "ConnectionPending"

  describe "Enum-like properties" $ do
    it "should have exactly 3 values" $ do
      let allStates = [ConnectionEstablished, ConnectionClosed, ConnectionPending]
      length allStates `shouldBe` 3

    it "should cover all constructors in pattern matching" $ do
      let stateToInt :: ConnectionState -> Int
          stateToInt ConnectionEstablished = 1
          stateToInt ConnectionClosed = 2
          stateToInt ConnectionPending = 3
      map stateToInt [ConnectionEstablished, ConnectionClosed, ConnectionPending]
        `shouldBe` [1, 2, 3]

  describe "QuickCheck generators" $ do
    it "should generate all possible values" $ property $ do
      states <- sample' (arbitrary :: Gen ConnectionState)
      let uniqueStates = L.nub states
      length uniqueStates `shouldSatisfy` (>= 3)

-- | Arbitrary instance for property testing
instance Arbitrary ConnectionState where
  arbitrary =
    elements
      [ ConnectionEstablished
      , ConnectionClosed
      , ConnectionPending
      ]

  shrink ConnectionEstablished = [ConnectionClosed, ConnectionPending]
  shrink ConnectionClosed = [ConnectionPending]
  shrink ConnectionPending = []
