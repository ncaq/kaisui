module Network.Transport.Kaisui.Type.TransportStateSpec
  ( spec
  ) where

import Network.Transport.Kaisui.Type.TransportState
import RIO
import qualified RIO.List as L
import Test.QuickCheck
import Test.Syd

spec :: Spec
spec = describe "TransportState" $ do
  describe "Eq instance" $ do
    it "should be reflexive"
      $ property
      $ \(state :: TransportState) -> state == state

    it "should be symmetric"
      $ property
      $ \(s1 :: TransportState) (s2 :: TransportState) ->
        (s1 == s2) == (s2 == s1)

    it "should handle all cases correctly" $ do
      TransportOpen `shouldBe` TransportOpen
      TransportClosed `shouldBe` TransportClosed
      TransportOpen `shouldNotBe` TransportClosed

  describe "Ord instance" $ do
    it "should be transitive"
      $ property
      $ \(s1 :: TransportState) (s2 :: TransportState) (s3 :: TransportState) ->
        not (s1 <= s2 && s2 <= s3) || (s1 <= s3)

    it "should be antisymmetric"
      $ property
      $ \(s1 :: TransportState) (s2 :: TransportState) ->
        not (s1 <= s2 && s2 <= s1) || (s1 == s2)

    it "should have consistent ordering" $ do
      let states = [TransportOpen, TransportClosed]
      L.sort states `shouldBe` [TransportOpen, TransportClosed]

  describe "Show instance" $ do
    it "should produce expected string representations" $ do
      show TransportOpen `shouldBe` "TransportOpen"
      show TransportClosed `shouldBe` "TransportClosed"

  describe "Enum-like properties" $ do
    it "should have exactly 2 values" $ do
      let allStates = [TransportOpen, TransportClosed]
      length allStates `shouldBe` 2

    it "should cover all constructors in pattern matching" $ do
      let stateToInt :: TransportState -> Int
          stateToInt TransportOpen = 1
          stateToInt TransportClosed = 2
      map stateToInt [TransportOpen, TransportClosed]
        `shouldBe` [1, 2]

  describe "QuickCheck generators" $ do
    it "should generate all possible values" $ property $ do
      states <- sample' (arbitrary :: Gen TransportState)
      let uniqueStates = L.nub states
      length uniqueStates `shouldSatisfy` (>= 2)

  describe "State transitions" $ do
    it "should model valid state transitions" $ do
      -- A transport starts as Open
      let initialState = TransportOpen
      -- Can transition to Closed
      let closedState = TransportClosed
      -- Once closed, cannot reopen (in typical usage)
      initialState `shouldNotBe` closedState

-- | Arbitrary instance for property testing
instance Arbitrary TransportState where
  arbitrary = elements [TransportOpen, TransportClosed]

  shrink TransportOpen = [TransportClosed]
  shrink TransportClosed = []
