module Network.Transport.Kaisui.ProtocolSpec
  ( spec
  ) where

import Data.Convertible
import Network.Transport (EndPointAddress (..))
import qualified Network.Transport as NT
import Network.Transport.Kaisui.Protocol
import qualified Proto.Kaisui.ConnectionRequest as P
import qualified Proto.Kaisui.ConnectionResponse as P2
import Proto.Kaisui.Envelope
import RIO
import Test.Syd

spec :: Spec
spec = describe "Kaisui Protocol" $ do
  describe "encodeEnvelope and decodeEnvelope" $ do
    it "should encode and decode connection request" $ do
      let epAddr = EndPointAddress "test:8080:1"
          rel = NT.ReliableOrdered
          connId = 12345 :: NT.ConnectionId
          envelope = createConnectionRequest epAddr rel connId
          encoded = encodeEnvelope envelope

      case decodeEnvelope encoded of
        Left err -> expectationFailure $ "Decode failed: " <> show err
        Right decoded -> decoded `shouldBe` envelope

    it "should encode and decode connection response with success" $ do
      let connId = 12345 :: NT.ConnectionId
          epAddr = EndPointAddress "test:8080:1"
          envelope = createConnectionResponse connId (Right epAddr)
          encoded = encodeEnvelope envelope

      case decodeEnvelope encoded of
        Left err -> expectationFailure $ "Decode failed: " <> show err
        Right decoded -> decoded `shouldBe` envelope

    it "should encode and decode connection response with error" $ do
      let connId = 12345 :: NT.ConnectionId
          errMsg = "Connection failed" :: Text
          envelope = createConnectionResponse connId (Left errMsg)
          encoded = encodeEnvelope envelope

      case decodeEnvelope encoded of
        Left err -> expectationFailure $ "Decode failed: " <> show err
        Right decoded -> decoded `shouldBe` envelope

    it "should encode and decode data message" $ do
      let connId = 12345 :: NT.ConnectionId
          payload = "Hello, World!" :: ByteString
          envelope = createDataMessage connId payload
          encoded = encodeEnvelope envelope

      case decodeEnvelope encoded of
        Left err -> expectationFailure $ "Decode failed: " <> show err
        Right decoded -> decoded `shouldBe` envelope

    it "should encode and decode close connection" $ do
      let connId = 12345 :: NT.ConnectionId
          envelope = createCloseConnection connId
          encoded = encodeEnvelope envelope

      case decodeEnvelope encoded of
        Left err -> expectationFailure $ "Decode failed: " <> show err
        Right decoded -> decoded `shouldBe` envelope

    it "should encode and decode close endpoint" $ do
      let epAddr = EndPointAddress "test:8080:1"
          envelope = createCloseEndPoint epAddr
          encoded = encodeEnvelope envelope

      case decodeEnvelope encoded of
        Left err -> expectationFailure $ "Decode failed: " <> show err
        Right decoded -> decoded `shouldBe` envelope

  describe "reliability encoding" $ do
    it "should encode ReliableOrdered as 1" $ do
      let envelope = createConnectionRequest (EndPointAddress "test:8080:1") NT.ReliableOrdered 1
      case envelope ^. message of
        Just (ConnectionRequestMessage req) -> req ^. P.reliability `shouldBe` 1
        _ -> expectationFailure "Expected ConnectionRequestMessage"

    it "should encode ReliableUnordered as 2" $ do
      let envelope = createConnectionRequest (EndPointAddress "test:8080:1") NT.ReliableUnordered 1
      case envelope ^. message of
        Just (ConnectionRequestMessage req) -> req ^. P.reliability `shouldBe` 2
        _ -> expectationFailure "Expected ConnectionRequestMessage"

    it "should encode Unreliable as 0" $ do
      let envelope = createConnectionRequest (EndPointAddress "test:8080:1") NT.Unreliable 1
      case envelope ^. message of
        Just (ConnectionRequestMessage req) -> req ^. P.reliability `shouldBe` 0
        _ -> expectationFailure "Expected ConnectionRequestMessage"

  describe "message field extraction" $ do
    it "should correctly set connection ID in request" $ do
      let connId = 9876 :: NT.ConnectionId
          envelope = createConnectionRequest (EndPointAddress "test:8080:1") NT.ReliableOrdered connId
      case envelope ^. message of
        Just (ConnectionRequestMessage req) -> req ^. P.connectionId `shouldBe` convert (show connId)
        _ -> expectationFailure "Expected ConnectionRequestMessage"

    it "should correctly set endpoint address in request" $ do
      let epAddr = EndPointAddress "myhost:1234:5"
          envelope = createConnectionRequest epAddr NT.ReliableOrdered 1
      case envelope ^. message of
        Just (ConnectionRequestMessage req) ->
          EndPointAddress (convert (req ^. P.fromEndpoint)) `shouldBe` epAddr
        _ -> expectationFailure "Expected ConnectionRequestMessage"

    it "should correctly set success result in response" $ do
      let epAddr = EndPointAddress "result:5678:9"
          envelope = createConnectionResponse 1 (Right epAddr)
      case envelope ^. message of
        Just (ConnectionResponseMessage resp) ->
          case resp ^. P2.result of
            Just (P2.EndpointAddressResult addr) ->
              EndPointAddress (convert addr) `shouldBe` epAddr
            _ -> expectationFailure "Expected EndpointAddressResult"
        _ -> expectationFailure "Expected ConnectionResponseMessage"

    it "should correctly set error result in response" $ do
      let errMsg = "Test error message" :: Text
          envelope = createConnectionResponse 1 (Left errMsg)
      case envelope ^. message of
        Just (ConnectionResponseMessage resp) ->
          case resp ^. P2.result of
            Just (P2.ErrorResult err) -> err `shouldBe` errMsg
            _ -> expectationFailure "Expected ErrorResult"
        _ -> expectationFailure "Expected ConnectionResponseMessage"
