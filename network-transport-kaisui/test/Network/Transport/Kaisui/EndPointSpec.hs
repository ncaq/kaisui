module Network.Transport.Kaisui.EndPointSpec
  ( spec
  ) where

import Data.Convertible
import qualified Network.Socket as NS
import Network.Transport (ConnectionId, EndPointAddress (..))
import qualified Network.Transport as NT
import Network.Transport.Kaisui.Connection
import Network.Transport.Kaisui.EndPoint
import Network.Transport.Kaisui.Type.Connection
import Network.Transport.Kaisui.Type.ConnectionState
import Network.Transport.Kaisui.Type.EndPoint
import RIO
import qualified RIO.HashMap as HM
import Test.QuickCheck
import Test.Syd

spec :: Spec
spec = describe "Kaisui EndPoint" $ do
  describe "createKaisuiEndPoint" $ do
    it "should create an endpoint with given parameters" $ do
      sock <- NS.socket NS.AF_INET NS.Stream NS.defaultProtocol
      let addr = EndPointAddress "localhost:8080:1"
          queueSize = 100

      ep <- atomically $ createKaisuiEndPoint addr sock queueSize

      -- Verify fields
      ep ^. endpointId `shouldBe` addr
      ep ^. Network.Transport.Kaisui.Type.EndPoint.socket `shouldBe` sock

      -- Verify initial state
      conns <- readTVarIO (ep ^. connections)
      HM.null conns `shouldBe` True

      isClosed <- readTVarIO (ep ^. closed)
      isClosed `shouldBe` False

      -- Cleanup
      NS.close sock

    it "should create endpoints with different queue sizes"
      $ property
      $ \(NonNegative qSize) -> do
        sock <- NS.socket NS.AF_INET NS.Stream NS.defaultProtocol
        let addr = EndPointAddress "localhost:8080:1"
            queueSize = max 1 qSize -- Ensure at least 1
        ep <- atomically $ createKaisuiEndPoint addr sock queueSize

        -- Queue should be empty initially
        isEmpty <- atomically $ isEmptyTBQueue (ep ^. receiveQueue)
        isEmpty `shouldBe` True

        -- Cleanup
        NS.close sock

  describe "closeKaisuiEndPoint" $ do
    it "should close an endpoint and mark it as closed" $ do
      sock <- NS.socket NS.AF_INET NS.Stream NS.defaultProtocol
      let addr = EndPointAddress "localhost:8080:1"

      ep <- atomically $ createKaisuiEndPoint addr sock 100

      -- Close the endpoint
      closeKaisuiEndPoint ep

      -- Verify closed flag
      isClosed <- readTVarIO (ep ^. closed)
      isClosed `shouldBe` True

    it "should close all connections when closing endpoint" $ do
      sock <- NS.socket NS.AF_INET NS.Stream NS.defaultProtocol
      let epAddr = EndPointAddress "localhost:8080:1"

      ep <- atomically $ createKaisuiEndPoint epAddr sock 100

      -- Create some connections
      sock1 <- NS.socket NS.AF_INET NS.Stream NS.defaultProtocol
      sock2 <- NS.socket NS.AF_INET NS.Stream NS.defaultProtocol
      let sockAddr = NS.SockAddrInet 8080 (NS.tupleToHostAddress (127, 0, 0, 1))

      conn1 <- atomically $ do
        c <- newKaisuiConnection (1 :: ConnectionId) NT.ReliableOrdered sock1 sockAddr
        modifyTVar' (ep ^. connections) (HM.insert (1 :: ConnectionId) c)
        pure c

      conn2 <- atomically $ do
        c <- newKaisuiConnection (2 :: ConnectionId) NT.ReliableOrdered sock2 sockAddr
        modifyTVar' (ep ^. connections) (HM.insert (2 :: ConnectionId) c)
        pure c

      -- Close endpoint (should close all connections)
      closeKaisuiEndPoint ep

      -- Verify connections are closed
      state1 <- readTVarIO (conn1 ^. state)
      state2 <- readTVarIO (conn2 ^. state)
      state1 `shouldBe` ConnectionClosed
      state2 `shouldBe` ConnectionClosed

    it "should be idempotent" $ do
      sock <- NS.socket NS.AF_INET NS.Stream NS.defaultProtocol
      let addr = EndPointAddress "localhost:8080:1"

      ep <- atomically $ createKaisuiEndPoint addr sock 100

      -- Close multiple times
      closeKaisuiEndPoint ep
      closeKaisuiEndPoint ep
      closeKaisuiEndPoint ep

      -- Should still be closed
      isClosed <- readTVarIO (ep ^. closed)
      isClosed `shouldBe` True

  describe "receiveEvent" $ do
    it "should receive events from the queue" $ do
      sock <- NS.socket NS.AF_INET NS.Stream NS.defaultProtocol
      let addr = EndPointAddress "localhost:8080:1"

      ep <- atomically $ createKaisuiEndPoint addr sock 100

      -- Put an event in the queue
      let testEvent = NT.EndPointClosed
      atomically $ writeTBQueue (ep ^. receiveQueue) testEvent

      -- Receive the event
      receivedEvent <- receiveEvent ep
      receivedEvent `shouldBe` testEvent

      -- Cleanup
      NS.close sock

    it "should receive events in FIFO order" $ do
      sock <- NS.socket NS.AF_INET NS.Stream NS.defaultProtocol
      let addr = EndPointAddress "localhost:8080:1"

      ep <- atomically $ createKaisuiEndPoint addr sock 100

      -- Put multiple events
      let events =
            [ NT.EndPointClosed
            , NT.ConnectionOpened (1 :: ConnectionId) NT.ReliableOrdered (EndPointAddress "remote:9000:1")
            , NT.ReceivedMulticast
                (NT.MulticastAddress "multicast-group-1")
                ["data1", "data2"]
            ]

      atomically $ mapM_ (writeTBQueue (ep ^. receiveQueue)) events

      -- Receive events in order
      received1 <- receiveEvent ep
      received2 <- receiveEvent ep
      received3 <- receiveEvent ep

      [received1, received2, received3] `shouldBe` events

      -- Cleanup
      NS.close sock

    it "should block when queue is empty" $ do
      sock <- NS.socket NS.AF_INET NS.Stream NS.defaultProtocol
      let addr = EndPointAddress "localhost:8080:1"

      ep <- atomically $ createKaisuiEndPoint addr sock 100

      -- Try to receive with timeout
      result <- timeout 100000 $ receiveEvent ep -- 100ms timeout
      result `shouldBe` Nothing

      -- Cleanup
      NS.close sock

  describe "connection management" $ do
    it "should allow adding and removing connections" $ do
      sock <- NS.socket NS.AF_INET NS.Stream NS.defaultProtocol
      let addr = EndPointAddress "localhost:8080:1"

      ep <- atomically $ createKaisuiEndPoint addr sock 100

      -- Add a connection
      connSock <- NS.socket NS.AF_INET NS.Stream NS.defaultProtocol
      let sockAddr = NS.SockAddrInet 8080 (NS.tupleToHostAddress (127, 0, 0, 1))
          cid = 123 :: ConnectionId

      _ <- atomically $ do
        c <- newKaisuiConnection cid NT.ReliableOrdered connSock sockAddr
        modifyTVar' (ep ^. connections) (HM.insert cid c)
        pure c

      -- Verify connection exists
      conns <- readTVarIO (ep ^. connections)
      HM.member cid conns `shouldBe` True

      -- Remove connection
      atomically $ modifyTVar' (ep ^. connections) (HM.delete cid)

      -- Verify connection removed
      conns' <- readTVarIO (ep ^. connections)
      HM.member cid conns' `shouldBe` False

      -- Cleanup
      NS.close sock
      NS.close connSock

  describe "QuickCheck properties" $ do
    it "endpoint address should be immutable"
      $ property
      $ \(host :: String) (port :: Word16) (epId :: Word32) -> do
        sock <- NS.socket NS.AF_INET NS.Stream NS.defaultProtocol
        let addrStr = host <> ":" <> show port <> ":" <> show epId
            addr = EndPointAddress $ encodeUtf8 (convert addrStr :: Text)

        ep <- atomically $ createKaisuiEndPoint addr sock 100

        ep ^. endpointId `shouldBe` addr

        -- Cleanup
        NS.close sock

    it "closed flag should only change from False to True"
      $ property
      $ \(n :: Positive Int) -> do
        sock <- NS.socket NS.AF_INET NS.Stream NS.defaultProtocol
        let addr = EndPointAddress "localhost:8080:1"
            closeTimes = getPositive n

        ep <- atomically $ createKaisuiEndPoint addr sock 100

        -- Initial state
        isClosed1 <- readTVarIO (ep ^. closed)
        isClosed1 `shouldBe` False

        -- Close multiple times
        replicateM_ closeTimes $ closeKaisuiEndPoint ep

        -- Final state
        isClosed2 <- readTVarIO (ep ^. closed)
        isClosed2 `shouldBe` True
