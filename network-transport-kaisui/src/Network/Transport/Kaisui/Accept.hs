module Network.Transport.Kaisui.Accept
  ( acceptLoop
  ) where

import Data.Convertible
import Network.Socket hiding (socket)
import qualified Network.Socket.ByteString as NSB
import qualified Network.Transport as NT
import Network.Transport.Kaisui.Connection
import Network.Transport.Kaisui.Error
import Network.Transport.Kaisui.Protocol
import Network.Transport.Kaisui.Receive
import Network.Transport.Kaisui.Type.Connection
import Network.Transport.Kaisui.Type.EndPoint
import qualified Proto.Kaisui.ConnectionRequest as CR
import Proto.Kaisui.Envelope
import RIO
import qualified RIO.HashMap as HM

-- | Accept loop for incoming connections
acceptLoop :: (HasLogFunc env, MonadReader env m, MonadThrow m, MonadUnliftIO m) => KaisuiEndPoint -> m ()
acceptLoop ep = forever $ do
  acceptResult <- try $ liftIO $ accept (ep ^. socket)
  case acceptResult of
    Left (e :: SomeException) -> do
      -- Check if this is an expected error (endpoint closed)
      -- If the socket is closed, we exit the loop by throwing the exception
      -- Otherwise, log a warning and continue
      isClosed <- readTVarIO (ep ^. closed)
      if isClosed
        then throwM e -- Re-throw to exit the loop gracefully
        else do
          logWarn $ "Accept failed with unexpected error: " <> displayShow e
          -- Brief delay before retrying to avoid tight error loops
          threadDelay 100000 -- 100ms
    Right (sock, addr) -> withAsync (handleIncoming ep sock addr) link

-- | Handle incoming connection (inner logic)
handleIncoming
  :: (HasLogFunc env, MonadReader env m, MonadThrow m, MonadUnliftIO m) => KaisuiEndPoint -> Socket -> SockAddr -> m ()
handleIncoming ep sock addr = do
  bs <- liftIO $ NSB.recv sock 4096
  case decodeEnvelope bs of
    Left err -> liftIO $ do
      close sock
      throwDecodingFailed "Failed to decode envelope" err
    Right envelope -> handleEnvelope ep sock addr envelope

-- | Handle decoded envelope
handleEnvelope
  :: (HasLogFunc env, MonadReader env m, MonadThrow m, MonadUnliftIO m)
  => KaisuiEndPoint -> Socket -> SockAddr -> Envelope -> m ()
handleEnvelope ep sock addr envelope =
  case envelope ^. message of
    Just (ConnectionRequestMessage req) -> handleConnectionRequest ep sock addr req
    other -> liftIO $ do
      close sock
      throwUnexpectedMessage $ "Expected ConnectionRequestMessage, but got: " <> utf8BuilderToText (displayShow other)

-- | Handle connection request
handleConnectionRequest
  :: (HasLogFunc env, MonadReader env m, MonadThrow m, MonadUnliftIO m)
  => KaisuiEndPoint -> Socket -> SockAddr -> CR.ConnectionRequest -> m ()
handleConnectionRequest ep sock addr req = do
  cid <- parseConnectionId req sock
  let rel = parseReliability req
  conn <- registerConnection ep cid rel sock addr
  sendConnectionResponse sock cid (ep ^. endpointId)
  queueConnectionEvent ep cid rel req
  connectionReceiveLoop ep conn

-- | Parse connection ID from request
parseConnectionId
  :: (MonadIO m) => CR.ConnectionRequest -> Socket -> m NT.ConnectionId
parseConnectionId req sock = do
  let cidStr = convert $ req ^. CR.connectionId :: String
  case readMaybe cidStr of
    Nothing -> liftIO $ do
      close sock
      throwUnexpectedMessage $ "Invalid connection ID: " <> convert cidStr
    Just c -> pure c

-- | Parse reliability from request
parseReliability :: CR.ConnectionRequest -> NT.Reliability
parseReliability req =
  case req ^. CR.reliability of
    0 -> NT.Unreliable
    1 -> NT.ReliableOrdered
    _ -> NT.ReliableUnordered

-- | Register a new connection
registerConnection
  :: (MonadIO m) => KaisuiEndPoint -> NT.ConnectionId -> NT.Reliability -> Socket -> SockAddr -> m KaisuiConnection
registerConnection ep cid rel sock addr =
  atomically $ do
    connection <- newKaisuiConnection cid rel sock addr
    modifyTVar (ep ^. connections) (HM.insert cid connection)
    pure connection

-- | Send connection response
sendConnectionResponse
  :: (MonadIO m) => Socket -> NT.ConnectionId -> NT.EndPointAddress -> m ()
sendConnectionResponse sock cid epId = do
  let response = createConnectionResponse cid (Right epId)
  liftIO $ NSB.sendAll sock (encodeEnvelope response)

-- | Queue connection opened event
queueConnectionEvent
  :: (MonadIO m) => KaisuiEndPoint -> NT.ConnectionId -> NT.Reliability -> CR.ConnectionRequest -> m ()
queueConnectionEvent ep cid rel req =
  atomically
    $ writeTBQueue (ep ^. receiveQueue)
    $ NT.ConnectionOpened cid rel (NT.EndPointAddress $ convert $ req ^. CR.fromEndpoint)
