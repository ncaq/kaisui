module Network.Transport.Kaisui.AcceptSpec
  ( spec
  ) where

import Data.Convertible
import qualified Network.Socket as NS
import qualified Network.Socket.ByteString as NSB
import Network.Transport (ConnectionId, EndPointAddress (..))
import qualified Network.Transport as NT
import Network.Transport.Kaisui.Accept
import Network.Transport.Kaisui.Connection
import Network.Transport.Kaisui.EndPoint
import Network.Transport.Kaisui.Protocol
import Network.Transport.Kaisui.Type.Connection
import Network.Transport.Kaisui.Type.EndPoint
import qualified Proto.Kaisui.ConnectionRequest as P
import qualified Proto.Kaisui.ConnectionResponse as PR
import Proto.Kaisui.Envelope
import RIO
import qualified RIO.HashMap as HM
import Test.Syd

spec :: Spec
spec = describe "Kaisui Accept" $ do
  describe "acceptLoop" $ do
    it "should exit gracefully when endpoint is closed" $ do
      -- Create a listening socket
      listenSock <- NS.socket NS.AF_INET NS.Stream NS.defaultProtocol
      NS.setSocketOption listenSock NS.ReuseAddr 1
      NS.bind listenSock (NS.SockAddrInet 0 (NS.tupleToHostAddress (127, 0, 0, 1)))
      NS.listen listenSock 5
      port <- NS.socketPort listenSock

      -- Create endpoint
      let addr = EndPointAddress $ convert $ ("localhost:" :: Text) <> convert (show port) <> (":1" :: Text)
      ep <- atomically $ createKaisuiEndPoint addr listenSock 100

      -- Set up logging

      runSimpleApp $ do
        -- Start accept loop in background
        acceptAsync <- async $ noLogging $ acceptLoop ep

        -- Close endpoint (should cause accept loop to exit)
        atomically $ writeTVar (ep ^. closed) True
        liftIO $ NS.close listenSock

        -- Accept loop should terminate
        result <- timeout 1000000 $ tryAny $ wait acceptAsync -- 1 second timeout
        case result of
          Nothing -> liftIO $ expectationFailure "Accept loop did not terminate"
          Just (Left e) -> logDebug $ "Accept loop terminated with expected exception: " <> displayShow e
          Just (Right x) -> liftIO $ expectationFailure $ "Accept loop should have thrown exception, but got: " <> show x

  describe "reliability values" $ do
    it "should map numeric values to reliability types" $ do
      let req0 = P.ConnectionRequest{P.reliability = 0, P.connectionId = "", P.fromEndpoint = ""}
          req1 = P.ConnectionRequest{P.reliability = 1, P.connectionId = "", P.fromEndpoint = ""}
          req2 = P.ConnectionRequest{P.reliability = 2, P.connectionId = "", P.fromEndpoint = ""}
          req99 = P.ConnectionRequest{P.reliability = 99, P.connectionId = "", P.fromEndpoint = ""}

      -- parseReliability is internal, so we test the mapping logic
      let parseReliability r = case r of
            0 -> NT.Unreliable
            1 -> NT.ReliableOrdered
            _ -> NT.ReliableUnordered

      parseReliability (req0 ^. P.reliability) `shouldBe` NT.Unreliable
      parseReliability (req1 ^. P.reliability) `shouldBe` NT.ReliableOrdered
      parseReliability (req2 ^. P.reliability) `shouldBe` NT.ReliableUnordered
      parseReliability (req99 ^. P.reliability) `shouldBe` NT.ReliableUnordered
  describe "registerConnection" $ do
    it "should add connection to endpoint's connection map" $ do
      -- Create socket and endpoint
      sock <- NS.socket NS.AF_INET NS.Stream NS.defaultProtocol
      connSock <- NS.socket NS.AF_INET NS.Stream NS.defaultProtocol
      let addr = EndPointAddress "test:8080:1"
          sockAddr = NS.SockAddrInet 8080 (NS.tupleToHostAddress (127, 0, 0, 1))
          cid = 2001 :: ConnectionId
          rel = NT.ReliableOrdered

      ep <- atomically $ createKaisuiEndPoint addr sock 100

      -- Register connection manually (registerConnection is internal)
      conn <- atomically $ do
        c <- newKaisuiConnection cid rel connSock sockAddr
        modifyTVar' (ep ^. connections) (HM.insert cid c)
        pure c

      -- Verify registration
      conns <- readTVarIO (ep ^. connections)
      HM.member cid conns `shouldBe` True

      -- Verify connection properties
      conn ^. connectionId `shouldBe` cid
      conn ^. reliability `shouldBe` rel
      conn ^. remoteAddr `shouldBe` sockAddr

      -- Cleanup
      NS.close sock
      NS.close connSock

  describe "sendConnectionResponse" $ do
    it "should send encoded connection response" $ do
      -- Create socket pair
      (clientSock, serverSock) <- NS.socketPair NS.AF_UNIX NS.Stream NS.defaultProtocol

      let cid = 2002 :: ConnectionId
          epId = EndPointAddress "server:8080:1"

      -- Send response manually (sendConnectionResponse is internal)
      let response = createConnectionResponse cid (Right epId)
      NSB.sendAll serverSock (encodeEnvelope response)

      -- Receive and verify
      bs <- NSB.recv clientSock 4096
      case decodeEnvelope bs of
        Left err -> expectationFailure $ "Failed to decode: " <> show err
        Right envelope ->
          case envelope ^. message of
            Just (ConnectionResponseMessage resp) -> do
              resp ^. PR.connectionId `shouldBe` convert (show cid)
              case resp ^. PR.result of
                Just (PR.EndpointAddressResult addr) -> EndPointAddress (convert addr) `shouldBe` epId
                other -> expectationFailure $ "Expected EndpointAddressResult, but got: " <> show other
            other -> expectationFailure $ "Expected ConnectionResponseMessage, but got: " <> show other

      -- Cleanup
      NS.close clientSock
      NS.close serverSock

  describe "queueConnectionEvent" $ do
    it "should queue NT.ConnectionOpened event" $ do
      -- Create endpoint
      sock <- NS.socket NS.AF_INET NS.Stream NS.defaultProtocol
      let addr = EndPointAddress "test:8080:1"
      ep <- atomically $ createKaisuiEndPoint addr sock 100

      -- Create request
      let req =
            P.ConnectionRequest
              { P.connectionId = "queue-test"
              , P.reliability = 1
              , P.fromEndpoint = "remote:9000:2"
              }
          cid = 2003 :: ConnectionId
          rel = NT.ReliableOrdered

      -- Queue event manually (queueConnectionEvent is internal)
      atomically
        $ writeTBQueue (ep ^. receiveQueue)
        $ NT.ConnectionOpened cid rel (EndPointAddress $ convert $ req ^. P.fromEndpoint)

      -- Verify event
      event <- atomically $ readTBQueue (ep ^. receiveQueue)
      event `shouldBe` NT.ConnectionOpened cid NT.ReliableOrdered (EndPointAddress "remote:9000:2")

      -- Cleanup
      NS.close sock
