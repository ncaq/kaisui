module Network.Transport.Kaisui.ConnectSpec
  ( spec
  ) where

import Data.Convertible
import qualified Network.Socket as NS
import qualified Network.Socket.ByteString as NSB
import Network.Transport (ConnectionId, EndPointAddress (..))
import qualified Network.Transport as NT
import Network.Transport.Kaisui.Connect
import Network.Transport.Kaisui.EndPoint
import Network.Transport.Kaisui.Protocol
import Network.Transport.Kaisui.TestHelper (TestReliability (..))
import Network.Transport.Kaisui.Type.EndPoint
import Proto.Kaisui.ConnectionRequest
import Proto.Kaisui.Envelope
import RIO
import qualified RIO.HashMap as HM
import Test.QuickCheck
import Test.Syd

spec :: Spec
spec = describe "Kaisui Connect" $ do
  describe "connectKaisui" $ do
    it "should fail with invalid address format" $ do
      sock <- NS.socket NS.AF_INET NS.Stream NS.defaultProtocol
      let epAddr = EndPointAddress "localhost:8080:1"
      ep <- atomically $ createKaisuiEndPoint epAddr sock 100

      runSimpleApp $ do
        -- Try to connect with invalid address
        result <- connectKaisui ep (EndPointAddress "invalid-address") NT.ReliableOrdered NT.defaultConnectHints

        case result of
          Left (NT.TransportError NT.ConnectFailed msg) ->
            liftIO $ msg `shouldContain` "Invalid address format"
          Right _conn -> liftIO $ expectationFailure "Expected ConnectFailed error, but got successful connection"
          Left (NT.TransportError code msg) -> liftIO $ expectationFailure $ "Expected ConnectFailed error, but got: " <> show code <> " - " <> msg

      -- Cleanup
      NS.close sock

    it "should fail when cannot connect to remote" $ do
      sock <- NS.socket NS.AF_INET NS.Stream NS.defaultProtocol
      let epAddr = EndPointAddress "localhost:8080:1"
      ep <- atomically $ createKaisuiEndPoint epAddr sock 100

      runSimpleApp $ do
        -- Try to connect to non-existent address
        result <- connectKaisui ep (EndPointAddress "localhost:60000:1") NT.ReliableOrdered NT.defaultConnectHints

        case result of
          Left (NT.TransportError NT.ConnectFailed _) -> pure ()
          Right _conn -> liftIO $ expectationFailure "Expected ConnectFailed error, but got successful connection"
          Left (NT.TransportError code msg) -> liftIO $ expectationFailure $ "Expected ConnectFailed error, but got: " <> show code <> " - " <> msg

      -- Cleanup
      NS.close sock

    it "should establish connection with mock server" $ do
      -- Create server socket
      serverSock <- NS.socket NS.AF_INET NS.Stream NS.defaultProtocol
      NS.setSocketOption serverSock NS.ReuseAddr 1
      NS.bind serverSock (NS.SockAddrInet 0 (NS.tupleToHostAddress (127, 0, 0, 1)))
      NS.listen serverSock 5
      port <- NS.socketPort serverSock

      -- Create client endpoint
      clientSock <- NS.socket NS.AF_INET NS.Stream NS.defaultProtocol
      let epAddr = EndPointAddress "localhost:8081:1"
      ep <- atomically $ createKaisuiEndPoint epAddr clientSock 100

      runSimpleApp $ do
        -- Start mock server
        serverAsync <- async $ do
          (connSock, _) <- liftIO $ NS.accept serverSock
          -- Read connection request
          bs <- liftIO $ NSB.recv connSock 4096
          case decodeEnvelope bs of
            Left err -> logDebug $ "Mock server failed to decode envelope: " <> displayShow err
            Right envelope ->
              case envelope ^. message of
                Just (ConnectionRequestMessage _) -> do
                  -- Send simple response (not fully valid, but enough for test)
                  let resp = createConnectionResponse (3001 :: ConnectionId) (Right epAddr)
                  liftIO $ NSB.sendAll connSock (encodeEnvelope resp)
                other -> logWarn $ "Expected ConnectionRequestMessage, but got: " <> displayShow other
          -- Keep socket open briefly
          threadDelay 100000
          liftIO $ NS.close connSock

        -- Connect to mock server
        let remoteAddr = EndPointAddress $ convert $ ("localhost:" :: Text) <> convert (show port) <> (":2" :: Text)
        result <- connectKaisui ep remoteAddr NT.ReliableOrdered NT.defaultConnectHints

        case result of
          Left err -> liftIO $ expectationFailure $ "Connection failed: " <> show err
          Right conn -> do
            -- Verify connection was registered
            conns <- readTVarIO (ep ^. connections)
            liftIO $ HM.size conns `shouldBe` 1

            -- Close connection
            liftIO $ NT.close conn

        -- Cleanup
        cancel serverAsync

      -- Cleanup
      NS.close serverSock
      NS.close clientSock

  describe "reliability handling" $ do
    it "should handle different reliability modes"
      $ property
      $ \(TestReliability rel) -> do
        sock <- NS.socket NS.AF_INET NS.Stream NS.defaultProtocol
        let epAddr = EndPointAddress "localhost:8080:1"
        ep <- atomically $ createKaisuiEndPoint epAddr sock 100

        runSimpleApp $ do
          -- Try invalid connection (but verify it processes reliability)
          result <- connectKaisui ep (EndPointAddress "invalid") rel NT.defaultConnectHints
          case result of
            Left err -> logDebug $ "Connection failed as expected: " <> displayShow err
            Right conn -> liftIO $ do
              NT.close conn
              expectationFailure "Expected connection to fail with invalid address"

        -- Cleanup
        NS.close sock

  -- createSocket and registerNewConnection are internal functions, not testing directly

  describe "connection request message format" $ do
    it "should create proper connection request envelope" $ do
      -- Create socket pair
      (clientSock, serverSock) <- NS.socketPair NS.AF_UNIX NS.Stream NS.defaultProtocol

      let epId = EndPointAddress "client:8080:1"
          rel = NT.ReliableOrdered
          connId = 3003 :: ConnectionId

      -- Send request manually (sendConnectionRequest is internal)
      let request = createConnectionRequest epId rel connId
      NSB.sendAll clientSock (encodeEnvelope request)

      -- Receive and verify
      bs <- NSB.recv serverSock 4096
      case decodeEnvelope bs of
        Left err -> expectationFailure $ "Failed to decode: " <> show err
        Right envelope ->
          case envelope ^. message of
            Just (ConnectionRequestMessage req) -> do
              EndPointAddress (convert (req ^. fromEndpoint)) `shouldBe` epId
              req ^. Proto.Kaisui.ConnectionRequest.reliability `shouldBe` 1 -- NT.ReliableOrdered
              req ^. Proto.Kaisui.ConnectionRequest.connectionId `shouldBe` convert (show connId)
            _ -> expectationFailure "Expected ConnectionRequestMessage"

      -- Cleanup
      NS.close clientSock
      NS.close serverSock
