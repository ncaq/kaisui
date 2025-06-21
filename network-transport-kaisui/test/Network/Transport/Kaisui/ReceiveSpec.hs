module Network.Transport.Kaisui.ReceiveSpec
  ( spec
  ) where

import Data.Convertible
import qualified Network.Socket as NS
import qualified Network.Socket.ByteString as NSB
import Network.Transport (ConnectionId, EndPointAddress (..))
import qualified Network.Transport as NT
import Network.Transport.Kaisui.Connection
import Network.Transport.Kaisui.EndPoint
import Network.Transport.Kaisui.Protocol
import Network.Transport.Kaisui.Receive
import Network.Transport.Kaisui.Type.Connection
import Network.Transport.Kaisui.Type.ConnectionState
import Network.Transport.Kaisui.Type.EndPoint
import qualified Proto.Kaisui.CloseConnection as P
import qualified Proto.Kaisui.DataMessage as P
import Proto.Kaisui.Envelope
import RIO
import qualified RIO.HashMap as HM
import Test.Syd

spec :: Spec
spec = describe "Kaisui Receive" $ do
  describe "connectionReceiveLoop" $ do
    it "should handle data messages" $ do
      -- Create sockets
      (clientSock, serverSock) <- NS.socketPair NS.AF_UNIX NS.Stream NS.defaultProtocol

      -- Create endpoint and connection
      let epAddr = EndPointAddress "test:8080:1"
          sockAddr = NS.SockAddrUnix "test"
          cid = 4001 :: ConnectionId
          rel = NT.ReliableOrdered

      ep <- atomically $ createKaisuiEndPoint epAddr serverSock 100
      conn <- atomically $ do
        c <- newKaisuiConnection cid rel clientSock sockAddr
        modifyTVar' (ep ^. connections) (HM.insert cid c)
        pure c

      runSimpleApp $ do
        -- Start receive loop in background
        recvAsync <- async $ connectionReceiveLoop ep conn

        -- Send data message
        let dataMsg =
              P.DataMessage
                { P.connectionId = convert (show cid)
                , P.payload = "Hello, World!"
                }
            envelope = Envelope{message = Just (DataMessageMessage dataMsg)}
        liftIO $ NSB.sendAll serverSock (encodeEnvelope envelope)

        -- Receive event
        event <- atomically $ readTBQueue (ep ^. receiveQueue)
        liftIO $ event `shouldBe` NT.Received cid ["Hello, World!"]

        -- Cleanup
        cancel recvAsync

      -- Cleanup sockets
      NS.close clientSock
      NS.close serverSock

    it "should handle connection closed by peer" $ do
      -- Create sockets
      (clientSock, serverSock) <- NS.socketPair NS.AF_UNIX NS.Stream NS.defaultProtocol

      -- Create endpoint and connection
      let epAddr = EndPointAddress "test:8080:1"
          sockAddr = NS.SockAddrUnix "test"
          cid = 4002 :: ConnectionId
          rel = NT.ReliableOrdered

      ep <- atomically $ createKaisuiEndPoint epAddr serverSock 100
      conn <- atomically $ do
        c <- newKaisuiConnection cid rel clientSock sockAddr
        modifyTVar' (ep ^. connections) (HM.insert cid c)
        pure c

      runSimpleApp $ do
        -- Start receive loop
        recvAsync <- async $ connectionReceiveLoop ep conn

        -- Close the server side to simulate peer disconnect
        liftIO $ NS.close serverSock

        -- Should receive NT.ConnectionClosed event
        event <- atomically $ readTBQueue (ep ^. receiveQueue)
        liftIO $ event `shouldBe` NT.ConnectionClosed cid

        -- Verify connection was removed
        conns <- readTVarIO (ep ^. connections)
        liftIO $ HM.member cid conns `shouldBe` False

        -- Verify connection state
        currentState <- readTVarIO (conn ^. state)
        liftIO $ currentState `shouldBe` ConnectionClosed

        -- Receive loop should have exited normally
        result <- tryAny $ wait recvAsync
        case result of
          Left e -> liftIO $ expectationFailure $ "Receive loop threw unexpected exception: " <> show e
          Right v -> logDebug $ "Receive loop exited normally with: " <> displayShow v

      -- Cleanup
      NS.close clientSock

    it "should handle close connection message" $ do
      -- Create sockets
      (clientSock, serverSock) <- NS.socketPair NS.AF_UNIX NS.Stream NS.defaultProtocol

      -- Create endpoint and connection
      let epAddr = EndPointAddress "test:8080:1"
          sockAddr = NS.SockAddrUnix "test"
          cid = 4003 :: ConnectionId
          rel = NT.ReliableOrdered

      ep <- atomically $ createKaisuiEndPoint epAddr serverSock 100
      conn <- atomically $ do
        c <- newKaisuiConnection cid rel clientSock sockAddr
        modifyTVar' (ep ^. connections) (HM.insert cid c)
        pure c

      runSimpleApp $ do
        -- Start receive loop
        recvAsync <- async $ connectionReceiveLoop ep conn

        -- Send close connection message
        let closeMsg = P.CloseConnection{P.connectionId = convert $ show cid}
            envelope = Envelope{message = Just (CloseConnectionMessage closeMsg)}
        liftIO $ NSB.sendAll serverSock (encodeEnvelope envelope)

        -- Should receive NT.ConnectionClosed event
        event <- atomically $ readTBQueue (ep ^. receiveQueue)
        liftIO $ event `shouldBe` NT.ConnectionClosed cid

        -- Verify connection was removed
        conns <- readTVarIO (ep ^. connections)
        liftIO $ HM.member cid conns `shouldBe` False

        -- Receive loop should exit successfully
        result <- tryAny $ wait recvAsync
        case result of
          Left e -> liftIO $ expectationFailure $ "Receive loop threw unexpected exception: " <> show e
          Right v -> logDebug $ "Receive loop exited normally with: " <> displayShow v

      -- Cleanup
      NS.close clientSock
      NS.close serverSock

    it "should handle invalid envelope data" $ do
      -- Create sockets
      (clientSock, serverSock) <- NS.socketPair NS.AF_UNIX NS.Stream NS.defaultProtocol

      -- Create endpoint and connection
      let epAddr = EndPointAddress "test:8080:1"
          sockAddr = NS.SockAddrUnix "test"
          cid = 4004 :: ConnectionId
          rel = NT.ReliableOrdered

      ep <- atomically $ createKaisuiEndPoint epAddr serverSock 100
      conn <- atomically $ do
        c <- newKaisuiConnection cid rel clientSock sockAddr
        modifyTVar' (ep ^. connections) (HM.insert cid c)
        pure c

      runSimpleApp $ do
        -- Start receive loop
        recvAsync <- async $ connectionReceiveLoop ep conn

        -- Send invalid data
        liftIO $ NSB.sendAll serverSock "invalid protobuf data"

        -- Should eventually close the connection
        event <- atomically $ readTBQueue (ep ^. receiveQueue)
        liftIO $ event `shouldBe` NT.ConnectionClosed cid

        -- Receive loop should have exited with error
        result <- tryAny $ wait recvAsync
        case result of
          Left e -> logDebug $ "Receive loop exited with expected exception: " <> displayShow e
          Right v -> liftIO $ expectationFailure $ "Receive loop should have thrown exception but returned: " <> show v

      -- Cleanup
      NS.close clientSock
      NS.close serverSock

    it "should handle multiple data messages" $ do
      -- Create sockets
      (clientSock, serverSock) <- NS.socketPair NS.AF_UNIX NS.Stream NS.defaultProtocol

      -- Create endpoint and connection
      let epAddr = EndPointAddress "test:8080:1"
          sockAddr = NS.SockAddrUnix "test"
          cid = 4005 :: ConnectionId
          rel = NT.ReliableOrdered

      ep <- atomically $ createKaisuiEndPoint epAddr serverSock 100
      conn <- atomically $ do
        c <- newKaisuiConnection cid rel clientSock sockAddr
        modifyTVar' (ep ^. connections) (HM.insert cid c)
        pure c

      runSimpleApp $ do
        -- Start receive loop
        recvAsync <- async $ connectionReceiveLoop ep conn

        -- Send multiple messages
        let messages = ["Message 1", "Message 2", "Message 3"] :: [Text]
        forM_ messages $ \msg -> do
          let dataMsg =
                P.DataMessage
                  { P.connectionId = convert (show cid)
                  , P.payload = convert msg
                  }
              envelope = Envelope{message = Just (DataMessageMessage dataMsg)}
          liftIO $ NSB.sendAll serverSock (encodeEnvelope envelope)
          threadDelay 10000 -- Small delay between messages

        -- Receive all events
        receivedMessages <- forM messages $ \_ -> do
          event <- atomically $ readTBQueue (ep ^. receiveQueue)
          case event of
            NT.Received _ [payload] -> pure $ convert payload
            _ -> liftIO $ expectationFailure $ "Unexpected event: " <> show event

        liftIO $ receivedMessages `shouldBe` messages

        -- Cleanup
        cancel recvAsync

      -- Cleanup
      NS.close clientSock
      NS.close serverSock

    it "should handle unexpected message types" $ do
      -- Create sockets
      (clientSock, serverSock) <- NS.socketPair NS.AF_UNIX NS.Stream NS.defaultProtocol

      -- Create endpoint and connection
      let epAddr = EndPointAddress "test:8080:1"
          sockAddr = NS.SockAddrUnix "test"
          cid = 4006 :: ConnectionId
          rel = NT.ReliableOrdered

      ep <- atomically $ createKaisuiEndPoint epAddr serverSock 100
      conn <- atomically $ do
        c <- newKaisuiConnection cid rel clientSock sockAddr
        modifyTVar' (ep ^. connections) (HM.insert cid c)
        pure c

      runSimpleApp $ do
        -- Start receive loop
        recvAsync <- async $ connectionReceiveLoop ep conn

        -- Send a ConnectionRequest message (unexpected in data stream)
        let wrongMsg = createConnectionRequest (EndPointAddress "wrong:9000:1") NT.ReliableOrdered (4007 :: ConnectionId)
        liftIO $ NSB.sendAll serverSock (encodeEnvelope wrongMsg)

        -- Should close the connection due to protocol error
        event <- atomically $ readTBQueue (ep ^. receiveQueue)
        liftIO $ event `shouldBe` NT.ConnectionClosed cid

        -- Receive loop should have exited with error
        result <- tryAny $ wait recvAsync
        case result of
          Left e -> logDebug $ "Receive loop exited with expected exception: " <> displayShow e
          Right v -> liftIO $ expectationFailure $ "Receive loop should have thrown exception but returned: " <> show v

      -- Cleanup
      NS.close clientSock
      NS.close serverSock
