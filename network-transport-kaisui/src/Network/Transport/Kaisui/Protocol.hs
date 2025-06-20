module Network.Transport.Kaisui.Protocol
  ( encodeEnvelope
  , decodeEnvelope
  , createConnectionRequest
  , createConnectionResponse
  , createDataMessage
  , createCloseConnection
  , createCloseEndPoint
  , encodeFrame
  , decodeFrame
  ) where

import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import Data.Convertible
import Network.Transport
import Network.Transport.Kaisui.Type.FrameHeader
import Network.Transport.Kaisui.Type.FrameType
import Proto.Kaisui.CloseConnection
import Proto.Kaisui.CloseEndPoint
import Proto.Kaisui.ConnectionRequest
import Proto.Kaisui.ConnectionResponse
import Proto.Kaisui.DataMessage (DataMessage (..))
import qualified Proto.Kaisui.DataMessage as DM
import Proto.Kaisui.Envelope (Envelope (..), EnvelopeMessage (..))
import qualified Proto3.Suite.Class as P
import RIO
import qualified RIO.ByteString as BS
import qualified RIO.ByteString.Lazy as LBS
import qualified RIO.Partial as RIO'
import qualified RIO.Text as T

-- | Encode envelope to ByteString using Protocol Buffers
encodeEnvelope :: Envelope -> ByteString
encodeEnvelope env = LBS.toStrict $ P.toLazyByteString env

-- | Decode envelope from ByteString using Protocol Buffers
decodeEnvelope :: ByteString -> Either ByteString Envelope
decodeEnvelope bs = first (convert . T.pack . show) (P.fromByteString bs)

-- | Create connection request
createConnectionRequest :: EndPointAddress -> Reliability -> ConnectionId -> Envelope
createConnectionRequest (EndPointAddress addr) rel cid =
  Envelope
    $ Just
    $ ConnectionRequestMessage
    $ ConnectionRequest
      { fromEndpoint = convert addr
      , reliability = case rel of
          ReliableOrdered -> 1
          ReliableUnordered -> 2
          Unreliable -> 0
      , connectionId = convert (show cid)
      }

-- | Convert EndPointAddress to ConnectionResponseResult
endPointAddressToResult :: EndPointAddress -> ConnectionResponseResult
endPointAddressToResult (EndPointAddress addr) = EndpointAddressResult $ convert addr

-- | Create connection response
createConnectionResponse :: ConnectionId -> Either Text EndPointAddress -> Envelope
createConnectionResponse cid res =
  Envelope
    $ Just
    $ ConnectionResponseMessage
    $ ConnectionResponse
      { connectionId = convert (show cid)
      , result = Just $ either ErrorResult endPointAddressToResult res
      }

-- | Create data message
createDataMessage :: ConnectionId -> ByteString -> Envelope
createDataMessage cid pld =
  Envelope
    $ Just
    $ DataMessageMessage
    $ DataMessage
      { DM.connectionId = convert (show cid)
      , DM.payload = pld
      }

-- | Create close connection message
createCloseConnection :: ConnectionId -> Envelope
createCloseConnection cid =
  Envelope
    $ Just
    $ CloseConnectionMessage
    $ CloseConnection
      { connectionId = convert (show cid)
      }

-- | Create close endpoint message
createCloseEndPoint :: EndPointAddress -> Envelope
createCloseEndPoint (EndPointAddress addr) =
  Envelope
    $ Just
    $ CloseEndPointMessage
    $ CloseEndPoint
      { endpointId = convert addr
      }

-- | Encode a frame with header
encodeFrame :: FrameType -> ByteString -> ByteString
encodeFrame ftype payload =
  let header = runPut $ do
        putWord8 (convert $ fromEnum ftype)
        putWord32be (convert $ BS.length payload)
   in LBS.toStrict header <> payload

-- | Decode frame header
decodeFrame :: ByteString -> Either Text (FrameHeader, ByteString)
decodeFrame bs
  | BS.length bs < 5 = Left "Insufficient data for frame header"
  | otherwise =
      case runGetOrFail getFrameHeader (convert bs) of
        Left (_, _, err) -> Left (convert err)
        Right (rest, _, header) ->
          let payloadLen = convert $ header ^. frameLength :: Int
              restBS = LBS.toStrict rest
           in if BS.length restBS < payloadLen
                then Left "Insufficient data for payload"
                else Right (header, BS.take payloadLen restBS)
 where
  getFrameHeader = do
    typeVal <- getWord8
    let ftype = RIO'.toEnum (convert typeVal) :: FrameType
    FrameHeader ftype <$> getWord32be
