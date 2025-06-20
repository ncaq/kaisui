module Network.Transport.Kaisui.TestHelper
  ( withTestTransport
  , withTestEndPoint
  , withTestConnection
  , TestReliability (..)
  , getReliability
  )
where

import Network.Transport
import Network.Transport.Kaisui.Transport
import RIO
import Test.QuickCheck

-- | Newtype wrapper for Reliability to avoid orphan instance
newtype TestReliability = TestReliability {unTestReliability :: Reliability}
  deriving (Eq, Show)

-- | Extract the Reliability value
getReliability :: TestReliability -> Reliability
getReliability (TestReliability rel) = rel

instance Arbitrary TestReliability where
  arbitrary = TestReliability <$> elements [ReliableOrdered, ReliableUnordered, Unreliable]
  shrink (TestReliability ReliableOrdered) = []
  shrink (TestReliability ReliableUnordered) = [TestReliability ReliableOrdered]
  shrink (TestReliability Unreliable) = [TestReliability ReliableOrdered, TestReliability ReliableUnordered]

-- | Test transport setup helper
withTestTransport :: (Transport -> IO a) -> IO a
withTestTransport action = do
  transport <- createTransport "127.0.0.1" "0"
  finally (action transport) (closeTransport transport)

-- | Test endpoint setup helper
withTestEndPoint :: Transport -> (EndPoint -> IO a) -> IO a
withTestEndPoint transport action = do
  endpoint <- newEndPoint transport
  case endpoint of
    Left err -> throwString $ "Failed to create endpoint: " <> show err
    Right ep -> finally (action ep) (closeEndPoint ep)

-- | Test connection setup helper
withTestConnection
  :: EndPoint
  -> EndPointAddress
  -> Reliability
  -> ConnectHints
  -> (Connection -> IO a)
  -> IO a
withTestConnection endpoint addr reliability hints action = do
  conn <- connect endpoint addr reliability hints
  case conn of
    Left err -> throwString $ "Failed to create connection: " <> show err
    Right c -> finally (action c) (close c)
