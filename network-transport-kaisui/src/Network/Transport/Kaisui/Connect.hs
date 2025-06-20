module Network.Transport.Kaisui.Connect
  ( connectKaisui
  ) where

import Data.Convertible
import qualified Network.Socket as NS
import qualified Network.Socket.ByteString as NSB
import Network.Transport as NT
import Network.Transport.Kaisui.Adapter
import Network.Transport.Kaisui.Address
import Network.Transport.Kaisui.Connection
import Network.Transport.Kaisui.Protocol
import Network.Transport.Kaisui.Receive
import Network.Transport.Kaisui.Type.Connection hiding (reliability, remoteAddr)
import Network.Transport.Kaisui.Type.EndPoint as EP
import RIO
import qualified RIO.HashMap as HM
import System.Random
import Text.Megaparsec

-- | Connect to remote endpoint
-- Note: Returns Either to conform to Network.Transport's EndPoint interface,
-- which requires: IO (Either (TransportError ConnectErrorCode) Connection)
-- MonadThrow is used internally for error handling in sub-components
connectKaisui
  :: (HasLogFunc env, MonadReader env m, MonadThrow m, MonadUnliftIO m)
  => KaisuiEndPoint
  -> EndPointAddress
  -> NT.Reliability
  -> NT.ConnectHints
  -- ^ Don't use this parameter, but keep it for interface compatibility.
  -> m (Either (NT.TransportError NT.ConnectErrorCode) Connection)
connectKaisui kep remoteAddr reliability _hints = do
  -- Parse remote address
  case parseAddress remoteAddr of
    Left err ->
      pure $ Left $ NT.TransportError NT.ConnectFailed $ "Invalid address format: " <> errorBundlePretty err
    Right (host, port, _) -> connectToAddress kep host port reliability

-- | Connect to a specific address
connectToAddress
  :: (HasLogFunc env, MonadReader env m, MonadThrow m, MonadUnliftIO m)
  => KaisuiEndPoint
  -> Text
  -> NS.PortNumber
  -> NT.Reliability
  -> m (Either (NT.TransportError NT.ConnectErrorCode) Connection)
connectToAddress kep host port reliability = do
  -- Create connection socket
  socketResult <- createSocket host port
  either (pure . Left) (\(sock, addr) -> establishConnection kep sock addr reliability) socketResult

-- | Create and connect socket
createSocket
  :: (MonadUnliftIO m)
  => Text
  -> NS.PortNumber
  -> m (Either (NT.TransportError NT.ConnectErrorCode) (NS.Socket, NS.AddrInfo))
createSocket host port = do
  socketConnectionResult <- RIO.try $ liftIO $ do
    let hints = NS.defaultHints{NS.addrSocketType = NS.Stream}
    addr : _ <- NS.getAddrInfo (Just hints) (Just $ convert host) (Just $ show port)
    sock <- NS.socket (NS.addrFamily addr) (NS.addrSocketType addr) (NS.addrProtocol addr)
    NS.connect sock (NS.addrAddress addr)
    pure (sock, addr)
  pure $ first (\(e :: SomeException) -> NT.TransportError NT.ConnectFailed (displayException e)) socketConnectionResult

-- | Establish connection after socket is created
establishConnection
  :: (HasLogFunc env, MonadReader env m, MonadThrow m, MonadUnliftIO m)
  => KaisuiEndPoint
  -> NS.Socket
  -> NS.AddrInfo
  -> NT.Reliability
  -> m (Either (NT.TransportError NT.ConnectErrorCode) Connection)
establishConnection kep sock addr reliability = do
  connId <- liftIO $ randomIO @Word64
  conn <- registerNewConnection kep connId reliability sock (NS.addrAddress addr)
  sendConnectionRequest sock (kep ^. EP.endpointId) reliability connId
  startReceiveLoop kep conn
  pure $ Right $ toConnection conn

-- | Register a new connection
registerNewConnection
  :: (MonadIO m)
  => KaisuiEndPoint
  -> NT.ConnectionId
  -> NT.Reliability
  -> NS.Socket
  -> NS.SockAddr
  -> m KaisuiConnection
registerNewConnection kep connId reliability sock addr =
  atomically $ do
    connection <- newKaisuiConnection connId reliability sock addr
    modifyTVar (kep ^. EP.connections) (HM.insert connId connection)
    pure connection

-- | Send connection request message
sendConnectionRequest
  :: (MonadIO m)
  => NS.Socket
  -> NT.EndPointAddress
  -> NT.Reliability
  -> NT.ConnectionId
  -> m ()
sendConnectionRequest sock epId reliability connId = do
  let envelope = createConnectionRequest epId reliability connId
  liftIO $ NSB.sendAll sock (encodeEnvelope envelope)

-- | Start receive loop for connection
startReceiveLoop
  :: (HasLogFunc env, MonadReader env m, MonadThrow m, MonadUnliftIO m)
  => KaisuiEndPoint
  -> KaisuiConnection
  -> m ()
startReceiveLoop kep conn = do
  recvAsync <- async $ connectionReceiveLoop kep conn
  link recvAsync
